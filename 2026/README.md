# #30DayMapChallenge 2026

Thirty maps in November, one a day, on the [official 2026 themes](https://30daymapchallenge.com/). The plan lives in [`days.yml`](days.yml); every day has a folder with its idea, data sources and prep, and finished maps show up in the gallery at **[brooksgroves.com/30DayMapChallenge/2026](https://brooksgroves.com/30DayMapChallenge/2026/)**.

Earlier years are in [`../2023`](../2023) and [`../2024`](../2024).

## Getting ready (October)

### Mapbox basemaps (this year's sponsor)

Days 1, 2 and 28 draw on a Mapbox basemap when a token is set, and fall back to plain outlines without one. Mapbox static maps are Web Mercator, carry the Mapbox wordmark, and get "© Mapbox © OpenStreetMap" in the credit line; `toolkit/basemap.py` does all three.

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

- Drafted from real data: **3, 4, 5, 28**.
- Script ready, waiting for your export: **1, 2**.
- To build in October, a few each week: the rest. Public-data days first (8, 9, 12, 15, 18, 22, 24, 25), then the ones that need your data or prep.

## Make a map

Each day's folder holds a `make.py` (or `make.R`) that writes `out/map.png` and `out/alt.txt`. The toolkit gives every map the same look as brooksgroves.com: parchment and ink, Playfair Display titles, a header with the day and theme, and a credit line.

```python
import sys; sys.path.insert(0, "../toolkit")
import dmc

fig, ax = dmc.figure("square")            # or "portrait" (4:5) or "wide" (16:9)
# ... plot on ax ...
dmc.frame(fig, 28, subtitle="One line about the map.", source="USGS")
dmc.save(fig, 28, alt="What the map shows, for screen readers.")
```

In R, `source("../toolkit/dmc.R")` gives the same palette, fonts and caption for ggplot2.

### On Windows (PowerShell)

```powershell
cd 2026
pixi install                  # Python + R + GDAL from conda-forge, the first time
pixi run render 28            # runs day-28-feeling/make.py, then rebuilds the plan
pixi run build                # just rebuild READMEs, days.json and thumbnails
```

### In GitHub

- **Build the 2026 plan** runs on every push to `2026/`: it rebuilds the day READMEs, `days.json` and thumbnails and commits them.
- **Render a 2026 map** (Actions → Run workflow → day number) runs that day's script in the cloud, where the data downloads happen, and commits the map. Handy when the laptop's network blocks a data source.

## When a map is done

1. Set the day's `status` to `done` in `days.yml` (or `posted`, with `post:` set to the link).
2. Check `out/alt.txt`: one or two sentences saying what the map shows.
3. Post with **#30DayMapChallenge** and the day's theme, the map, and the alt text.

The gallery picks it up from `days.json` within a few minutes.

## The month

<!-- table:start -->
**0 of 30 done** · 💡 idea 7 · 📦 data in hand 0 · ✏️ draft 23 · ✅ done 0 · 📣 posted 0

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
| 14 | Sat Nov 14 | Borgesian map | [1:1](day-14-borgesian-map/) | 💡 idea |
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
