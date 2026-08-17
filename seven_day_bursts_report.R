# =============================================================================
# SEVEN-DAY BURSTS — Last.fm "hottest 7-day stretch" report
# =============================================================================
# For each Last.fm account, this script finds — across the entirety of their
# scrobble history — the single 7-day window in which each Song, Artist, and
# Album was played the most times, then ranks those entities by that peak.
# Output is one self-contained interactive HTML file per user (safe to email,
# AirDrop, or share directly — no separate lib folder needed), plus a
# combined index.
#
# USAGE
#   1. Set env var LASTFM_API_KEY (a single key works for all usernames).
#   2. Run this script from Desktop/Project/LastFM/ (or edit BASE_DIR below).
#      It will create ./charts and NOT touch any existing project folders.
#   3. Re-running is incremental: each user's raw scrobbles are cached as
#      charts/data/<username>_scrobbles.rds, and only new plays since the
#      last cached timestamp are fetched on subsequent runs.
#
# OUTPUT
#   charts/
#     data/<username>_scrobbles.rds        <- raw cached scrobbles (per user)
#     results/<username>_bursts.rds        <- computed burst tables (per user)
#     Vjrupp49_seven_day_bursts.html       <- fully self-contained, shareable
#     curtisschaefer_seven_day_bursts.html
#     Coltboy11_seven_day_bursts.html
#     ellaschlag_seven_day_bursts.html
#     Izthewiz24_seven_day_bursts.html
#     gabejensen33_seven_day_bursts.html
#     index.html                            <- links to all reports
# =============================================================================

# ---- 0. Packages -----------------------------------------------------------
required_pkgs <- c("httr", "jsonlite", "dplyr", "tidyr", "purrr", "tibble",
                   "glue", "stringr", "reactable", "htmltools")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---- 1. Config ---------------------------------------------------------
USERNAMES <- c("Vjrupp49", "curtisschaefer", "Coltboy11", "ellaschlag", "Izthewiz24", "gabejensen33")
API_KEY   <- Sys.getenv("LASTFM_API_KEY")
if (identical(API_KEY, "")) stop("Set LASTFM_API_KEY as an environment variable before running.")

BASE_DIR       <- "charts"
DATA_DIR       <- file.path(BASE_DIR, "data")
RESULTS_DIR    <- file.path(BASE_DIR, "results")
TOP_N          <- 20      # how many entities to show per category
MIN_LIFETIME_PLAYS <- 3   # ignore near one-off tracks/artists/albums as noise
FORCE_REFRESH  <- FALSE   # set TRUE to ignore cache and re-pull everything

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 2. Fetch + cache scrobbles (incremental) -------------------------
fetch_user_scrobbles <- function(username, api_key, cache_dir,
                                 force_refresh = FALSE, page_limit = 200) {
  cache_file <- file.path(cache_dir, paste0(username, "_scrobbles.rds"))
  existing <- NULL
  max_known_uts <- 0
  
  if (file.exists(cache_file) && !force_refresh) {
    existing <- readRDS(cache_file)
    if (nrow(existing) > 0) max_known_uts <- max(existing$uts, na.rm = TRUE)
  }
  
  message(glue::glue("[{username}] fetching scrobbles (cached max uts: {max_known_uts}) ..."))
  base_url <- "http://ws.audioscrobbler.com/2.0/"
  page <- 1L
  total_pages <- NA_integer_
  new_rows <- list()
  
  repeat {
    resp <- httr::GET(base_url, query = list(
      method = "user.getrecenttracks", user = username, api_key = api_key,
      format = "json", limit = page_limit, page = page, extended = 0
    ))
    if (httr::status_code(resp) != 200) {
      warning(glue::glue("[{username}] API error on page {page}: HTTP {httr::status_code(resp)}"))
      break
    }
    parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
    tracks <- parsed$recenttracks$track
    if (is.null(tracks) || length(tracks) == 0 || nrow(tracks) == 0) break
    
    if (is.na(total_pages)) {
      total_pages <- suppressWarnings(as.integer(parsed$recenttracks$`@attr`$totalPages))
      if (is.na(total_pages)) total_pages <- page
    }
    
    # drop the "now playing" row, which has no date field
    if ("date.uts" %in% names(tracks)) {
      tracks <- tracks[!is.na(tracks$date.uts), , drop = FALSE]
    } else {
      tracks <- tracks[0, , drop = FALSE]
    }
    
    if (nrow(tracks) > 0) {
      df_page <- tibble::tibble(
        artist = tracks[["artist.#text"]],
        album  = if ("album.#text" %in% names(tracks)) tracks[["album.#text"]] else NA_character_,
        track  = tracks[["name"]],
        uts    = as.numeric(tracks[["date.uts"]])
      )
      if (max_known_uts > 0 && any(df_page$uts <= max_known_uts, na.rm = TRUE)) {
        df_page <- df_page[df_page$uts > max_known_uts, , drop = FALSE]
        new_rows[[length(new_rows) + 1]] <- df_page
        break  # caught up to cache; older pages are all already stored
      }
      new_rows[[length(new_rows) + 1]] <- df_page
    }
    
    if (page >= total_pages) break
    page <- page + 1L
    Sys.sleep(0.25)  # be polite to the API
  }
  
  new_df <- if (length(new_rows) > 0) {
    dplyr::bind_rows(new_rows)
  } else {
    tibble::tibble(artist = character(), album = character(), track = character(), uts = numeric())
  }
  
  combined <- dplyr::bind_rows(existing, new_df) %>%
    dplyr::distinct(artist, track, uts, .keep_all = TRUE) %>%
    dplyr::arrange(uts)
  
  saveRDS(combined, cache_file)
  message(glue::glue("[{username}] {nrow(combined)} total scrobbles cached ({nrow(new_df)} new)."))
  
  combined %>%
    dplyr::mutate(timestamp = as.POSIXct(uts, origin = "1970-01-01", tz = "UTC"))
}

# ---- 3. Core algorithm: max plays in any rolling 7-day window ---------
# Two-pointer sliding window over sorted timestamps -> O(n) per entity.
max_7day_burst <- function(times) {
  times <- sort(times)
  n <- length(times)
  if (n == 0) {
    return(tibble::tibble(peak_plays = 0L,
                          window_start = as.POSIXct(NA), window_end = as.POSIXct(NA)))
  }
  j <- 1L
  best_count <- 0L
  best_i <- 1L
  best_j <- 1L
  for (i in seq_len(n)) {
    if (j < i) j <- i
    while (j <= n && as.numeric(difftime(times[j], times[i], units = "days")) < 7) {
      j <- j + 1L
    }
    count <- j - i
    if (count > best_count) {
      best_count <- count
      best_i <- i
      best_j <- j - 1L
    }
  }
  tibble::tibble(peak_plays = best_count, window_start = times[best_i], window_end = times[best_j])
}

compute_top_bursts <- function(scrobbles, group_vars, top_n = TOP_N,
                               min_lifetime_plays = MIN_LIFETIME_PLAYS) {
  lifetime <- scrobbles %>%
    dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "lifetime_plays") %>%
    dplyr::filter(lifetime_plays >= min_lifetime_plays)
  
  scrobbles %>%
    dplyr::inner_join(lifetime, by = group_vars) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::group_modify(~ max_7day_burst(.x$timestamp)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(lifetime, by = group_vars) %>%
    dplyr::filter(peak_plays > 0) %>%
    dplyr::arrange(dplyr::desc(peak_plays), dplyr::desc(lifetime_plays)) %>%
    dplyr::slice_head(n = top_n)
}

# ---- 4. Reactable table builder (dark bathymetric theme) --------------
oceanic_theme <- reactable::reactableTheme(
  color = "#cdeef0", backgroundColor = "#0b1c2c",
  borderColor = "#173247", stripedColor = "#0f2537",
  highlightColor = "#153348",
  cellPadding = "9px 12px",
  style = list(fontFamily = "'Inter', -apple-system, BlinkMacSystemFont, sans-serif", fontSize = "14px"),
  headerStyle = list(backgroundColor = "#081521", color = "#5eead4", borderColor = "#173247",
                     fontWeight = 600, textTransform = "uppercase",
                     letterSpacing = "0.05em", fontSize = "11px")
)

build_burst_table <- function(df, entity_type) {
  if (nrow(df) == 0) {
    return(htmltools::tags$p("No qualifying data.", style = "color:#8fb4c7; padding:20px;"))
  }
  df <- df %>%
    dplyr::mutate(
      Rank = dplyr::row_number(),
      Window = paste0(format(window_start, "%b %d, %Y"), "  \u2192  ", format(window_end, "%b %d, %Y"))
    )
  df <- switch(entity_type,
               song   = df %>% dplyr::mutate(Entity = paste0(track, "  \u2014  ", artist)),
               artist = df %>% dplyr::mutate(Entity = artist),
               album  = df %>% dplyr::mutate(Entity = paste0(album, "  \u2014  ", artist))
  )
  df <- df %>%
    dplyr::select(Rank, Entity,
                  `Peak 7-Day Plays` = peak_plays,
                  Window,
                  `Lifetime Plays` = lifetime_plays)
  
  reactable::reactable(
    df,
    theme = oceanic_theme,
    defaultSorted = "Peak 7-Day Plays",
    defaultSortOrder = "desc",
    pagination = FALSE,
    highlight = TRUE,
    striped = TRUE,
    bordered = FALSE,
    columns = list(
      Rank = reactable::colDef(width = 55, align = "center",
                               style = list(color = "#5eead4", fontWeight = 700)),
      Entity = reactable::colDef(minWidth = 240),
      `Peak 7-Day Plays` = reactable::colDef(width = 150, align = "center",
                                             style = list(fontWeight = 700, color = "#38bdf8")),
      Window = reactable::colDef(width = 230, style = list(color = "#8fb4c7")),
      `Lifetime Plays` = reactable::colDef(width = 130, align = "center")
    )
  )
}

# ---- 5. Run pipeline per user -------------------------------------------
process_user <- function(username) {
  scrobbles <- fetch_user_scrobbles(username, API_KEY, DATA_DIR, force_refresh = FORCE_REFRESH)
  
  songs   <- compute_top_bursts(scrobbles, c("artist", "track"))
  artists <- compute_top_bursts(scrobbles, c("artist"))
  albums  <- scrobbles %>%
    dplyr::filter(!is.na(album), album != "") %>%
    compute_top_bursts(c("artist", "album"))
  
  result <- list(songs = songs, artists = artists, albums = albums,
                 total_scrobbles = nrow(scrobbles))
  saveRDS(result, file.path(RESULTS_DIR, paste0(username, "_bursts.rds")))
  result
}

message("== Running seven-day burst analysis for all accounts ==")
all_results <- purrr::set_names(USERNAMES) %>% purrr::map(process_user)

# ---- 6. Build one interactive HTML page per user -------------------------
page_css <- "
  body { margin:0; background:#081521; font-family:'Inter',-apple-system,sans-serif; color:#cdeef0; }
  .hero { padding:48px 40px 24px; background:linear-gradient(180deg,#0b1c2c 0%,#081521 100%);
          border-bottom:1px solid #173247; }
  .hero h1 { margin:0 0 6px; font-size:28px; letter-spacing:0.02em; color:#e6fbfa; }
  .hero .user { color:#5eead4; font-weight:700; }
  .hero p { margin:0; color:#8fb4c7; font-size:14px; }
  .wrap { max-width:960px; margin:0 auto; padding:32px 40px 60px; }
  .nav-tabs { border-bottom:1px solid #173247; margin-bottom:18px; }
  .nav-tabs .nav-link { color:#8fb4c7; border:none; border-bottom:3px solid transparent;
                         background:none; padding:10px 18px; font-weight:600; font-size:14px; }
  .nav-tabs .nav-link.active { color:#5eead4; border-bottom-color:#5eead4; background:none; }
  .tab-pane { padding-top:6px; }
  footer { text-align:center; color:#3f6072; font-size:12px; padding:20px; }
  a { color:#38bdf8; }
"

build_user_page <- function(username, result) {
  tab_id <- tolower(username)
  nav <- htmltools::tags$ul(
    class = "nav nav-tabs", id = paste0(tab_id, "-tabs"), role = "tablist",
    htmltools::tags$li(class = "nav-item", htmltools::tags$button(
      class = "nav-link active", `data-bs-toggle` = "tab",
      `data-bs-target` = paste0("#", tab_id, "-songs"), type = "button", "Songs")),
    htmltools::tags$li(class = "nav-item", htmltools::tags$button(
      class = "nav-link", `data-bs-toggle` = "tab",
      `data-bs-target` = paste0("#", tab_id, "-artists"), type = "button", "Artists")),
    htmltools::tags$li(class = "nav-item", htmltools::tags$button(
      class = "nav-link", `data-bs-toggle` = "tab",
      `data-bs-target` = paste0("#", tab_id, "-albums"), type = "button", "Albums"))
  )
  
  panes <- htmltools::tags$div(
    class = "tab-content",
    htmltools::tags$div(class = "tab-pane fade show active", id = paste0(tab_id, "-songs"),
                        htmltools::as.tags(build_burst_table(result$songs, "song"))),
    htmltools::tags$div(class = "tab-pane fade", id = paste0(tab_id, "-artists"),
                        htmltools::as.tags(build_burst_table(result$artists, "artist"))),
    htmltools::tags$div(class = "tab-pane fade", id = paste0(tab_id, "-albums"),
                        htmltools::as.tags(build_burst_table(result$albums, "album")))
  )
  
  doc <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title(glue::glue("{username} — 7-Day Bursts")),
      htmltools::tags$link(rel = "stylesheet",
                           href = "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/5.3.3/css/bootstrap.min.css"),
      htmltools::tags$style(page_css)
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "hero",
                          htmltools::tags$h1(htmltools::tags$span(class = "user", username), " — Seven-Day Bursts"),
                          htmltools::tags$p(glue::glue(
                            "Peak plays within any rolling 7-day window, across {format(result$total_scrobbles, big.mark=',')} total scrobbles."))
      ),
      htmltools::tags$div(class = "wrap", nav, panes),
      htmltools::tags$footer("Soundings project \u2014 Last.fm 7-day burst analysis"),
      htmltools::tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/5.3.3/js/bootstrap.bundle.min.js")
    )
  )
  doc
}

# ---- 6b. Self-contained save helper (THIS is the changed part) -----------
# Renders the widget normally (which creates a temp lib/ folder of JS/CSS),
# then inlines every LOCAL script/stylesheet directly into the HTML and
# discards the folder. Remote CDN links (Bootstrap) are left as-is since
# those load fine in any browser with internet access. Result: one file,
# safe to email/attach/share on its own.
#
# NOTE: this deliberately does NOT regex-scan the (growing) HTML string to
# find <script>/<link> tags. htmltools::save_html() already tells us exactly
# what local files it wrote (via libdir), so we walk that folder directly and
# do one fixed-string sub() per file. That avoids the previous approach's
# failure mode: re-running a tag-matching regex over the whole document after
# every replacement, which could pathologically re-match text *inside* an
# already-inlined minified JS bundle (react.min.js, the core-js shim, etc.)
# and made a single save take 10+ minutes instead of seconds.
inline_local_dependencies <- function(html_path, lib_dir) {
  html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  lib_name <- basename(lib_dir)

  # Paths relative to lib_dir, "/"-separated — exactly how htmltools writes
  # them into src="…"/href="…" attributes, regardless of host OS.
  rel_files <- list.files(lib_dir, recursive = TRUE)
  asset_files <- rel_files[grepl("\\.(js|css)$", rel_files, ignore.case = TRUE)]

  for (rel in asset_files) {
    fp <- file.path(lib_dir, rel)
    href <- paste0(lib_name, "/", rel)
    content <- paste(readLines(fp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

    if (grepl("\\.js$", rel, ignore.case = TRUE)) {
      old_tag <- paste0('<script src="', href, '"></script>')
      new_tag <- paste0("<script>\n", content, "\n</script>")
    } else {
      old_tag <- paste0('<link href="', href, '" rel="stylesheet" />')
      new_tag <- paste0("<style>\n", content, "\n</style>")
    }

    if (grepl(old_tag, html, fixed = TRUE)) {
      html <- sub(old_tag, new_tag, html, fixed = TRUE)
    } else {
      warning(glue::glue("[inline_local_dependencies] Expected tag not found for {href}; left as external reference."))
    }
  }

  writeLines(html, html_path, useBytes = TRUE)
}

save_shareable_html <- function(doc, out_file) {
  tmp_html <- tempfile(fileext = ".html")
  tmp_libdir <- tempfile("lib_")
  htmltools::save_html(doc, file = tmp_html, libdir = tmp_libdir)
  inline_local_dependencies(tmp_html, tmp_libdir)
  file.copy(tmp_html, out_file, overwrite = TRUE)
  unlink(c(tmp_html, tmp_libdir), recursive = TRUE)
}

for (username in USERNAMES) {
  doc <- build_user_page(username, all_results[[username]])
  out_file <- file.path(BASE_DIR, paste0(username, "_seven_day_bursts.html"))
  save_shareable_html(doc, out_file)
  message(glue::glue("Saved (shareable, single-file): {out_file}"))
}

# ---- 7. Combined index page ----------------------------------------------
index_doc <- htmltools::tags$html(
  htmltools::tags$head(
    htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$title("Seven-Day Bursts — Index"),
    htmltools::tags$style(page_css)
  ),
  htmltools::tags$body(
    htmltools::tags$div(class = "hero",
                        htmltools::tags$h1("Seven-Day Bursts"),
                        htmltools::tags$p("Peak 7-day play stretches for Song, Artist, and Album — pick an account.")
    ),
    htmltools::tags$div(class = "wrap",
                        htmltools::tags$ul(
                          style = "list-style:none; padding:0; font-size:18px; line-height:2.4;",
                          lapply(USERNAMES, function(u) {
                            htmltools::tags$li(htmltools::tags$a(
                              href = paste0(u, "_seven_day_bursts.html"),
                              style = "color:#5eead4; font-weight:700; text-decoration:none;", u))
                          })
                        )
    )
  )
)
htmltools::save_html(index_doc, file = file.path(BASE_DIR, "index.html"))

message("\n== Done. Each <username>_seven_day_bursts.html is now a single, shareable file. ==")
message("== Open charts/index.html locally to browse all reports together. ==")

