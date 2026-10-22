# Handoff: blog, science writing and #30DayMapChallenge (as of Oct 4, 2026)

For a new Claude chat to pick this work back up. Start by reading this file, then `git pull` both repos.

## Repos

| Repo | What it is |
|---|---|
| `bdgroves/bdgroves.github.io` | brooksgroves.com (GitHub Pages). Blog posts in `blog/`, images in `blog/img/<post>/`. |
| `bdgroves/30DayMapChallenge` | November 2026 map challenge (`2026/`), plus the reusable data tools: `gbif/` (GBIF downloads + analyses) and `geo/` (Copernicus DEM + OpenStreetMap features). |

Always `git pull --rebase` before working: bots and Actions commit often.

## To-do

### Waiting on Brooks
- [x] **Auto-posting keys** set 22 Oct (new X app "30DMC Map Poster", Read and write; Bluesky app password). The preview run signs in to both without posting: "signed in as @bdgroves" on X, signed in on Bluesky. Still check X Billing → Credits has a balance.
- [ ] **Mark days `done`** as they're finished; Days 27-30 (incl. the Day 30 napkin scan) before Nov 27.
- [ ] **Day 21 (30DayMapChallenge):** Brooks makes his OpenStreetMap edits, then re-render the "after" map.
- [ ] **Day 16:** waiting on the trailhead replies.
- [ ] **Delete two stale branches** in the GitHub web UI: `smoke-monitors` and `dmc-day2-runs` (harmless; the session can't delete branches).
- [ ] **Read the personal lines** in the two fish posts and tweak anything that doesn't sound like him; add which fish his UNR lab worked on if he remembers.

### Saved for next week
- [ ] Social posts (X, Bluesky, LinkedIn) for: smoke-vs-monitors, pikas, golden trout, Pyramid Lake. Tag forge3d's Milos as @milos_gis only for forge3d work.
- [ ] Square and portrait renders of the floods and water-year animations.
- [ ] Optional: Spokane caption note on the smoke post; a monitor-numbers version of the smoke animation.

### Ideas on the table
- [x] **Pikas part 2** is published (`blog/pikas-part-2-washington.html`, 6 Oct) and was **updated 19 Oct 2026**: Round Top (Dalquest's 14 MVZ pikas, pinned 105 km off in Spokane County; really the Selkirks Round Top, already re-found), Tye (old Wellington townsite, already re-found) and Bird Creek Meadows (5 iNat within 3 km) moved to re-found; 8 sites left to look at (GPX `blog/data/pika-wa-go-look.gpx`); Keechelus is the type locality of *brunnescens*; WDFW Pika Observation survey (Survey123) linked. Evidence: `gbif/pika/out/inat_wa_sites.json`, `gbif/pika/part2/`. Delete the `probe/dalquest` branch (scratch; can't be deleted from a cloud session).
- [ ] **More GBIF species:** mountain goat, white-tailed ptarmigan, marmots... (one JSON file each, see below).
- [ ] Golden trout heat shock angle: nobody seems to have measured hsp in golden trout; could be a follow-up.

## Published this session (all live)

| Post | Notes |
|---|---|
| `blog/icefields-flyover-post.html` | forge3d flyover, Lake Louise to the Columbia Icefield. Socials done. |
| `blog/wa-smoke-monitors-post.html` | HRRR-Smoke vs 56 AirNow PM2.5 monitors (reply to an X question). |
| `blog/whats-up-with-the-pikas.html` | GBIF pika records; 329 pre-1950 sites vs records since 2000. DOI 10.15468/dl.p7sdx7. Has a Listen section. |
| `blog/two-creeks-of-gold.html` | California golden trout. DOIs dl.p6g732, dl.p7ndd4. |
| `blog/survivors-of-an-inland-sea.html` | Pyramid Lake fish + Lake Lahontan map. DOIs dl.bf6u63, dl.pf96qt, dl.xddnek, dl.fmjyg6, dl.dv2epv. |

All three science posts have project cards on `ecology.html` (newest first: Pyramid Lake, golden trout, pika, then ANOLE-WATCH).

## 30DayMapChallenge status (`2026/`)
- Checklist in `2026/README.md`. Day 22 (projections, countries from maps.html) and Day 20 (Dipodomys hexagons, GBIF DOI 10.15468/dl.qyuad7) are done.
- Day 2 is "Back to the Seawall". **Never publish Garmin/Strava run tracks near home** (privacy decision; all run data removed).
- Day 21 is still the "before" map. Day 16 pending.
- Toolkit: `2026/toolkit/dmc.py` (figure/frame/save, house colours), `toolkit/build.py` regenerates READMEs, days.json, thumbs.

## Auto-posting in November (`2026/toolkit/post.py`, workflow `dmc-2026-post.yml`)

- Runs 8:07 AM Pacific every day in November (retry at 12:07 PM). Posts the day's map to X and
  Bluesky **only if its status in `days.yml` is `done`**; anything else is skipped with a warning.
- Text = the "Post it" text from `build.py` + the day's share link; image = `out/map.png` as a
  JPEG under 950 KB with `out/alt.txt` as alt text.
- `2026/posted.json` records each post per platform, so re-runs never double-post. When every
  platform with keys has posted, the status flips to `posted` and the gallery rebuilds.
- Preview without sending: Actions → "Post the day's map" → Run workflow (mode preview, day `all`),
  or `pixi run post-preview` locally.
- Keys: the same secret names as weather-report-bot (`TWITTER_API_KEY`, `TWITTER_API_SECRET`,
  `TWITTER_ACCESS_TOKEN`, `TWITTER_ACCESS_SECRET`, `BLUESKY_HANDLE`, `BLUESKY_APP_PASSWORD`);
  `X_*`/`BSKY_*` names also work. Secrets are per repo, so they must be set in this repo too.
  A platform without keys is skipped.
- X is pay-per-use (~$0.20 a post with a link, Feb 2026 pricing); 30 posts ≈ $6.
- Cruise: away from Nov 27, so Days 27-30 must be `done` before then.

## How the data tools work
- **GBIF:** add `gbif/requests/<slug>.json` (`name`, optional `synonyms`, `rank`, `kingdom`, `elevation: true/false`) and push. The "GBIF downloads" Action uses repo secrets `GBIF_USER/GBIF_PWD/GBIF_EMAIL`, waits for the DOI, and commits `gbif/downloads/<slug>.json` (key, DOI, citation) and `gbif/data/<slug>.csv.gz` (+ `dem_m` ground elevation). Matching refuses a broader rank so a subspecies can't pull a whole species.
- **Geo:** add `geo/requests/<slug>.json` (`kind: dem` with bounds/res/source 90|30, or `kind: osm` with an Overpass query using `{bbox}`) and push. The "Geo data" Action commits `geo/data/<slug>.tif|.geojson`. Existing: dem-lahontan, dem-pyramid, dem-kern, dem-west, osm-pyramid, osm-kern.
- **Analyses and figures:** `gbif/pika/`, `gbif/golden-trout/`, `gbif/lahontan/`, shared helpers in `gbif/mapkit.py` (house style, hillshade, OSM to shapes). Run with `FONTS=<dir with Roboto TTFs>` and `NE=<natural-earth-vector clone>`.

## Gotchas (cloud session)
- The container can't reach most of the web (GBIF API, Overpass, AirNow, Copernicus S3, Natural Earth CDN). GitHub works, so **network jobs run as GitHub Actions** triggered by pushing to their paths (workflow_dispatch via API is blocked).
- Actions logs/artifacts can't be downloaded: workflows commit their logs (`gbif/last-run.log`, `geo/last-run.log`).
- Natural Earth comes from a sparse clone of `nvkelso/natural-earth-vector`.
- Screenshot checks: Playwright with `/opt/pw-browsers/chromium-1194/chrome-linux/chrome`; start `python3 -m http.server 8765` with a pid file and kill by pid (never pkill).
- Never commit secrets: GBIF credentials live only in repo secrets; the FIRMS key file in wa-smoke must never be committed or shared.

## Site conventions
- Voice: friendly and warm (sharp or critical reads off-brand). Personal facts only as Brooks stated them: grew up in Groveland, CA; molecular biology at UNR (1990–94), worked with Dr. Lee Weber on heat shock proteins in fish.
- Science posts follow `blog/six-islands-six-lizards.html` / the pika post: kicker, tag pills, lede, h2 sections, `.photo-block` figures with alt text, `.caveat`, `.note` (method), `.pull`, and a "Go deeper" sources box (Listen / Read / Data). Tag `Science Writing`.
- New post = copy the previous post's head/header/reactions/footer, then add listings in three places:
  - `blog/index.html`: `<article class="post-item">` right after `  <div class="post-list">`.
  - `index.html`: a `writing-item` before the first existing one.
  - `tags.html`: a POSTS JSON line before the first `  {"url": "https://brooksgroves.com/blog/`.
- Charts: parchment `#f5f0e8`, ink `#1c1a16`, stone `#7a7268`; CVD-checked pair blue `#2e7aa6` / rust `#c8602e`, neutral `#b8b0a4`; Roboto in figures.
- Every number and citation gets checked by a separate agent before publishing.
- Commit trailer: `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>` plus the session link.
