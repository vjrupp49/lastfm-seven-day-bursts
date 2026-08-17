# Seven-Day Bursts

For each Last.fm account, this finds the single 7-day window where each song, artist, and album peaked hardest across the entire listening history — the "burst" that best captures the height of an obsession.

**[View the reports →](https://vjrupp49.github.io/lastfm-seven-day-bursts/)**

## What it does

`seven_day_bursts_report.R` pulls full scrobble history for a list of Last.fm accounts via the Last.fm API, then for every song, artist, and album a listener has played, slides a 7-day window across their history to find the stretch where that item got the most plays. The result is a per-account interactive report ranking those peak bursts — a quick way to see what someone was obsessed with, and exactly when.

Output is one self-contained interactive HTML file per user (built with Reactable, safe to email, AirDrop, or share directly — no separate library folder needed), plus a combined index linking all of them.

## Reports

- [Vjrupp49](https://vjrupp49.github.io/lastfm-seven-day-bursts/Vjrupp49_seven_day_bursts.html)
- [curtisschaefer](https://vjrupp49.github.io/lastfm-seven-day-bursts/curtisschaefer_seven_day_bursts.html)
- [Coltboy11](https://vjrupp49.github.io/lastfm-seven-day-bursts/Coltboy11_seven_day_bursts.html)
- [ellaschlag](https://vjrupp49.github.io/lastfm-seven-day-bursts/ellaschlag_seven_day_bursts.html)
- [Izthewiz24](https://vjrupp49.github.io/lastfm-seven-day-bursts/Izthewiz24_seven_day_bursts.html)
- [gabejensen33](https://vjrupp49.github.io/lastfm-seven-day-bursts/gabejensen33_seven_day_bursts.html)

## Running it yourself

Requires R with the Last.fm API and a `LASTFM_API_KEY` environment variable set. Edit the `USERNAMES` vector at the top of `seven_day_bursts_report.R` to the accounts you want, then run the script — it fetches and caches scrobble data, computes the burst windows, and renders the HTML reports and index automatically.

## Stack

R · Last.fm API · Reactable / htmlwidgets · GitHub Pages
