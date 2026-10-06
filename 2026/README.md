# #30DayMapChallenge 2026

Thirty maps in November, one a day, on the [official 2026 themes](https://30daymapchallenge.com/). The plan lives in [`days.yml`](days.yml); every day has a folder with its idea, data sources and prep, and finished maps show up in the gallery at **[brooksgroves.com/30DayMapChallenge/2026](https://brooksgroves.com/30DayMapChallenge/2026/)**.

Earlier years are in [`../2023`](../2023) and [`../2024`](../2024).

## Getting ready (October)

### Mapbox basemaps (this year's sponsor)

Mapbox basemaps go where streets, towns and water give the data its context: Light under days 1, 11, 12, 19, 23 and 28, Outdoors under days 2 and 9. Each falls back to plain outlines or the house shaded relief without a token. The rest keep their own backgrounds on purpose: the relief is the subject on days 3, 4 and 24, the elevation or depth data is the map on 5, 15 and 26, days 8, 20 and 22 need equal-area or azimuthal projections that Mapbox's Web Mercator can't give, day 6's basemap is Cook's chart, day 18's census blocks cover every inch, day 21 is drawn from raw OpenStreetMap, and day 27 is a dark density grid. Mapbox static maps are Web Mercator, carry the Mapbox wordmark, and get "© Mapbox © OpenStreetMap" in the credit line; `toolkit/basemap.py` does all three.

- [x] Make a free account at [account.mapbox.com](https://account.mapbox.com) and copy the **default public token** (starts `pk.`).
- [x] GitHub: repo **Settings → Secrets and variables → Actions → New repository secret**, name `MAPBOX_TOKEN`. (The drafts already show Mapbox basemaps.)
- [ ] On your PC, for renders there: `setx MAPBOX_TOKEN "pk...."` in PowerShell, then open a new window. Never put the token in a file in the repo.
- [ ] Optional, for the house look: Mapbox Studio → **New style → Upload**, choose `toolkit/mapbox/brooks-parchment.json`, publish, then add a repo **variable** (same page, Variables tab) `MAPBOX_STYLE` = `yourusername/styleid`. Until then maps use Mapbox Light.

### Data

- [x] **Day 1**: Geocaching *My Finds* pocket query. In (October 3); `finds.csv` is built from it.
- [x] **Day 23**: no Untappd export needed after all: drafted from the beer history HopLove keeps.
- [x] **Day 17** and **Day 29**: no Earthdata login needed. Day 17 uses the Black Marble composite NASA GIBS serves openly; Day 29 uses Sentinel-2 from AWS open data. (An Earthdata login would still let Day 17 use VNP46A4 annual radiance.)
- [x] **Day 20**: the citable GBIF download is in (DOI [10.15468/dl.qyuad7](https://doi.org/10.15468/dl.qyuad7)), and the map cites it.
- [ ] **Day 16**: paste each reply's trailhead into `answers.csv` (no handles). The question went out on X on October 3; the file is still empty.

### Things only you can do

- [x] **Day 6**: Cook's chart is georeferenced from its own graticule, with Tasman's coast and an inset of his chart.
- [x] **Day 11**: drafted with tsunami sirens; the lahar-siren version can come back if Pierce County shares locations.
- [ ] **Day 21**: an evening adding Groveland to OpenStreetMap, then `make.py --refresh`.
- [x] **Day 22**: all 37 countries from the Countries Visited layer on the maps page are in `places.csv`.
- [ ] **Day 27**: the lonboard tutorial, for the interactive version.
- [ ] **Day 30**: don't look at a map of Groveland in November.

### Where the maps stand

- Drafted from real data: **1–6, 8–15, 17–29** (13 and 14 are interactive pages, with stills for posts). Day 10's draft is the line-printer map; the real one is made from prompts on the day.
- Waiting on you: **7** (made on the day), **16** (replies into `answers.csv`), **30** (pen and paper).
- Before posting: **21** (re-render after your OSM evening).

<!-- table:start -->
**0 of 30 done** · 💡 idea 2 · 📦 data in hand 1 · ✏️ draft 27 · ✅ done 0 · 📣 posted 0

| Day | Date | Theme | Map | Status |
|---:|---|---|---|---|
| 1 | Sun Nov 1 | Points | [1,190 finds](day-01-points/) | ✏️ draft |
| 2 | Mon Nov 2 | Lines | [The Seawall](day-02-lines/) | ✏️ draft |
| 3 | Tue Nov 3 | Polygons | [Every fire around Groveland](day-03-polygons/) | ✏️ draft |
| 4 | Wed Nov 4 | Clusters | [Rainier's swarms](day-04-clusters/) | ✏️ draft |
| 5 | Thu Nov 5 | Sight | [Where you can see Rainier from](day-05-sight/) | ✏️ draft |
| 6 | Fri Nov 6 | Vintage | [Tasman to Cook](day-06-vintage/) | ✏️ draft |
| 7 | Sat Nov 7 | 10 minute map | [Ten minutes, on the clock](day-07-10-minute-map/) | 💡 idea |
| 8 | Sun Nov 8 | Utopia | [Paradise, Eden, Utopia](day-08-utopia/) | ✏️ draft |
| 9 | Mon Nov 9 | Urban-rural | [Tacoma to Paradise](day-09-urban-rural/) | ✏️ draft |
| 10 | Tue Nov 10 | Prompting only | [A map made only by asking](day-10-prompting-only/) | ✏️ draft |
| 11 | Wed Nov 11 | Sound | [Where the tsunami sirens reach](day-11-sound/) | ✏️ draft |
| 12 | Thu Nov 12 | Power | [The Tuolumne powers San Francisco](day-12-power/) | ✏️ draft |
| 13 | Fri Nov 13 | Interactions | [Follow a raindrop](day-13-interactions/) | ✏️ draft |
| 14 | Sat Nov 14 | Borgesian map | [1:1](day-14-borgesian-map/) | ✏️ draft |
| 15 | Sun Nov 15 | Inside out | [Inside Kīlauea](day-15-inside-out/) | ✏️ draft |
| 16 | Mon Nov 16 | Collaborative map | [Your favourite Sierra trailheads](day-16-collaborative-map/) | 📦 data in hand |
| 17 | Tue Nov 17 | Light & dark | [Great Basin's dark sky](day-17-light-and-dark/) | ✏️ draft |
| 18 | Wed Nov 18 | NULL | [Nobody lives here](day-18-null/) | ✏️ draft |
| 19 | Thu Nov 19 | Smell | [What the Yakima Valley smells like](day-19-smell/) | ✏️ draft |
| 20 | Fri Nov 20 | Hexagons | [Kangaroo rats in hexagons](day-20-hexagons/) | ✏️ draft |
| 21 | Sat Nov 21 | OpenStreetMap | [Groveland, by volunteers](day-21-openstreetmap/) | ✏️ draft |
| 22 | Sun Nov 22 | Projections | [The world from Groveland](day-22-projections/) | ✏️ draft |
| 23 | Mon Nov 23 | Taste | [Every brewery I've checked into](day-23-taste/) | ✏️ draft |
| 24 | Tue Nov 24 | Network | [Every stream that reaches Modesto](day-24-network/) | ✏️ draft |
| 25 | Wed Nov 25 | Is this a map? | [A day of ground motion](day-25-is-this-a-map/) | ✏️ draft |
| 26 | Thu Nov 26 | Water | [Lake Tahoe, clear and deep](day-26-water/) | ✏️ draft |
| 27 | Fri Nov 27 | New tool | [Every building in Washington](day-27-new-tool/) | ✏️ draft |
| 28 | Sat Nov 28 | Feeling | [Did you feel it? Nisqually, 2001](day-28-feeling/) | ✏️ draft |
| 29 | Sun Nov 29 | Raster | [When Rainier's snow melted](day-29-raster/) | ✏️ draft |
| 30 | Mon Nov 30 | Pen & paper | [If you hit the third cattle guard](day-30-pen-and-paper/) | 💡 idea |
<!-- table:end -->
