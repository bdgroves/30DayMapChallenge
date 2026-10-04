# #30DayMapChallenge 2026

Thirty maps in November, one a day, on the [official 2026 themes](https://30daymapchallenge.com/). The plan lives in [`days.yml`](days.yml); every day has a folder with its idea, data sources and prep, and finished maps show up in the gallery at **[brooksgroves.com/30DayMapChallenge/2026](https://brooksgroves.com/30DayMapChallenge/2026/)**.

Earlier years are in [`../2023`](../2023) and [`../2024`](../2024).

## Getting ready (October)

### Mapbox basemaps (this year's sponsor)

Mapbox basemaps go where streets, towns and water give the data its context: Light under days 1, 11, 12, 19, 23 and 28, Outdoors under days 2 and 9. Each falls back to plain outlines or the house shaded relief without a token. The rest keep their own backgrounds on purpose: the relief is the subject on days 3, 4 and 24, the elevation or depth data is the map on 5, 15 and 26, days 8, 20 and 22 need equal-area or azimuthal projections that Mapbox's Web Mercator can't give, day 6's basemap is Cook's chart, day 18's census blocks cover every inch, day 21 is drawn from raw OpenStreetMap, and day 27 is a dark density grid. Mapbox static maps are Web Mercator, carry the Mapbox wordmark, and get "© Mapbox © OpenStreetMap" in the credit line; `toolkit/basemap.py` does all three.

- [ ] Make a free account at [account.mapbox.com](https://account.mapbox.com) and copy the **default public token** (starts `pk.`).
- [ ] GitHub: repo **Settings → Secrets and variables → Actions → New repository secret**, name `MAPBOX_TOKEN`.
- [ ] On your PC, for renders there: `setx MAPBOX_TOKEN "pk...."` in PowerShell, then open a new window. Never put the token in a file in the repo.
- [ ] Optional, for the house look: Mapbox Studio → **New style → Upload**, choose `toolkit/mapbox/brooks-parchment.json`, publish, then add a repo **variable** (same page, Variables tab) `MAPBOX_STYLE` = `yourusername/styleid`. Until then maps use Mapbox Light.

### Data to request now (it takes time to arrive)

- [ ] **Day 1**: Geocaching *My Finds* pocket query (premium; once every 3 days). Save the .zip in `day-01-points/data/`.
- [ ] **Day 2**: Strava archive (Settings → My Account → Download your data; arrives by email). Unzip into `day-02-lines/data/`.
- [ ] **Day 20**: GBIF download of genus *Dipodomys* (free account at gbif.org).
- [ ] **Day 23**: Untappd check-in export (Supporter feature).
- [ ] **Day 17**: NASA Earthdata login works for Black Marble night lights.

### Things only you can do

- [ ] **Day 6**: find the Tasman (1642) and Cook (1769–70) charts and georeference them. The slowest prep of the month.
- [ ] **Day 11**: track down Pierce County's lahar siren locations (or settle on the fallback: Puget Sound by depth).
- [ ] **Day 16**: pick the collaborator, or open the trailhead form a week early.
- [ ] **Day 21**: an evening adding Groveland to OpenStreetMap.
- [ ] **Day 27**: the lonboard tutorial.
- [ ] **Day 30**: don't look at a map of Groveland in November.

### Where the maps stand

- Drafted from real data: **1–6, 8, 9, 11–15, 18–28** (13 and 14 are interactive pages, with stills for posts).
- Waiting on you: **7** (made on the day), **10** (prompts only, on the day), **16** (a collaborator), **17** and **29** (Earthdata login), **30** (pen and paper).
- Before posting: **20** needs a GBIF download with your account so the map can cite its DOI; **22** needs every place you've been in `places.csv`; **21** gets re-rendered (`make.py --refresh`) after your evening of OSM edits.

<!-- table:start -->
**0 of 30 done** · 💡 idea 6 · 📦 data in hand 0 · ✏️ draft 24 · ✅ done 0 · 📣 posted 0

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
| 10 | Tue Nov 10 | Prompting only | [A map made only by asking](day-10-prompting-only/) | 💡 idea |
| 11 | Wed Nov 11 | Sound | [Where the tsunami sirens reach](day-11-sound/) | ✏️ draft |
| 12 | Thu Nov 12 | Power | [The Tuolumne powers San Francisco](day-12-power/) | ✏️ draft |
| 13 | Fri Nov 13 | Interactions | [Follow a raindrop](day-13-interactions/) | ✏️ draft |
| 14 | Sat Nov 14 | Borgesian map | [1:1](day-14-borgesian-map/) | ✏️ draft |
| 15 | Sun Nov 15 | Inside out | [Inside Kīlauea](day-15-inside-out/) | ✏️ draft |
| 16 | Mon Nov 16 | Collaborative map | [Where we've been](day-16-collaborative-map/) | 💡 idea |
| 17 | Tue Nov 17 | Light & dark | [Great Basin's dark sky](day-17-light-and-dark/) | 💡 idea |
| 18 | Wed Nov 18 | NULL | [Nobody lives here](day-18-null/) | ✏️ draft |
| 19 | Thu Nov 19 | Smell | [What the Yakima Valley smells like](day-19-smell/) | ✏️ draft |
| 20 | Fri Nov 20 | Hexagons | [Kangaroo rats in hexagons](day-20-hexagons/) | ✏️ draft |
| 21 | Sat Nov 21 | OpenStreetMap | [Groveland, by volunteers](day-21-openstreetmap/) | ✏️ draft |
| 22 | Sun Nov 22 | Projections | [The world from Lakewood](day-22-projections/) | ✏️ draft |
| 23 | Mon Nov 23 | Taste | [Every brewery I've checked into](day-23-taste/) | ✏️ draft |
| 24 | Tue Nov 24 | Network | [Every stream that reaches Modesto](day-24-network/) | ✏️ draft |
| 25 | Wed Nov 25 | Is this a map? | [A day of ground motion](day-25-is-this-a-map/) | ✏️ draft |
| 26 | Thu Nov 26 | Water | [Lake Tahoe, clear and deep](day-26-water/) | ✏️ draft |
| 27 | Fri Nov 27 | New tool | [Every building in Washington](day-27-new-tool/) | ✏️ draft |
| 28 | Sat Nov 28 | Feeling | [Did you feel it? Nisqually, 2001](day-28-feeling/) | ✏️ draft |
| 29 | Sun Nov 29 | Raster | [Rainier's snow from space](day-29-raster/) | 💡 idea |
| 30 | Mon Nov 30 | Pen & paper | [Groveland from memory](day-30-pen-and-paper/) | 💡 idea |
<!-- table:end -->
