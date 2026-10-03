"""
Day 20 · Hexagons — Kangaroo rats in hexagons

Every georeferenced kangaroo rat (genus Dipodomys) record in GBIF: museum specimens back to the
1800s and today's iNaturalist sightings, binned into H3 hexagons (resolution 4, about 1,770 km²).

GBIF's search API returns at most 100,000 records per query, so the years are split until each
piece fits. No account needed for this; a GBIF download (with its DOI) is the citable version
and should replace this before the map is published.

Downloads (cached in data/): GBIF occurrence search, Natural Earth countries, Census states.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
import h3  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.colors import LogNorm  # noqa: E402
from shapely.geometry import Polygon  # noqa: E402

DAY = 20
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
API = "https://api.gbif.org/v1"
RES = 4
CRS = "+proj=lcc +lat_1=25 +lat_2=45 +lat_0=33 +lon_0=-110 +datum=WGS84 +units=m"
BASE = dict(hasCoordinate="true", hasGeospatialIssue="false", occurrenceStatus="PRESENT")

# ── records ──────────────────────────────────────────────────────────────────
cache = DATA / "dipodomys.csv.gz"
genus = fetch.json_get(f"{API}/species/match", dict(name="Dipodomys", rank="GENUS", kingdom="Animalia"))
key = genus["usageKey"]
total = fetch.json_get(f"{API}/occurrence/search", dict(genusKey=key, limit=0, **BASE))["count"]
print(f"= GBIF genus key {key}: {total:,} georeferenced records")


SMALL = 3000          # GBIF pages deep into a big result take a minute each; keep every query shallow


def count(y0, y1, month=None):
    q = dict(genusKey=key, year=f"{y0},{y1}", limit=0, **BASE)
    if month:
        q["month"] = month
    return fetch.json_get(f"{API}/occurrence/search", q)["count"]


def pieces(y0, y1):
    n = count(y0, y1)
    if n <= SMALL:
        return [(y0, y1, None, n)]
    if y0 == y1:                                   # one busy year: a month at a time
        return [(y0, y1, m, count(y0, y1, m)) for m in range(1, 13)]
    mid = (y0 + y1) // 2
    return pieces(y0, mid) + pieces(mid + 1, y1)


FIELDS = ["lat", "lon", "species", "year", "basis", "uncert_m", "country"]


def page(y0, y1, month, off):
    q = dict(genusKey=key, year=f"{y0},{y1}", limit=300, offset=off, **BASE)
    if month:
        q["month"] = month
    js = fetch.json_get(f"{API}/occurrence/search", q, timeout=120, tries=4)
    return [(r.get("decimalLatitude"), r.get("decimalLongitude"), r.get("species"), r.get("year"),
             r.get("basisOfRecord"), r.get("coordinateUncertaintyInMeters"), r.get("countryCode")) for r in js["results"]]


if not cache.exists():
    from concurrent.futures import ThreadPoolExecutor
    parts = []
    plan = pieces(1700, 2026)
    print(f"  {len(plan)} queries of at most {SMALL:,} records", flush=True)
    for y0, y1, month, n in plan:
        part = DATA / f"gbif_{y0}_{y1}_{month or 0}.csv.gz"     # each piece saved as it finishes
        if n and not part.exists():
            offs = list(range(0, n, 300))
            with ThreadPoolExecutor(6) as pool:
                rows = [r for pg in pool.map(lambda o: page(y0, y1, month, o), offs) for r in pg]
            pd.DataFrame(rows, columns=FIELDS).to_csv(part, index=False)
        if part.exists():
            parts.append(pd.read_csv(part))
    pd.concat(parts, ignore_index=True).to_csv(cache, index=False)
df = pd.read_csv(cache)
n_all = len(df)
df = df[df["lat"].notna() & df["lon"].notna() & ~((df["lat"] == 0) & (df["lon"] == 0))]
df = df[df["uncert_m"].isna() | (df["uncert_m"] <= 10_000)]
df = df[df["lon"].between(-130, -85) & df["lat"].between(14, 52)]
print(f"= {n_all:,} fetched, {len(df):,} kept (located to within 10 km, in North America); "
      f"{df['species'].nunique()} species; years {int(df['year'].min())}-{int(df['year'].max())}")
inat = (df["basis"] == "HUMAN_OBSERVATION").mean()
spec = (df["basis"] == "PRESERVED_SPECIMEN").mean()
print(f"= human observations {inat:.0%}, preserved specimens {spec:.0%}")

# ── hexagons ─────────────────────────────────────────────────────────────────
cell = getattr(h3, "latlng_to_cell", None) or h3.geo_to_h3
bound = getattr(h3, "cell_to_boundary", None) or h3.h3_to_geo_boundary
df["h"] = [cell(a, b, RES) for a, b in zip(df["lat"], df["lon"])]
hx = df.groupby("h").agg(n=("h", "size"), sp=("species", "nunique")).reset_index()
hx["geometry"] = [Polygon([(lo, la) for la, lo in bound(h)]) for h in hx["h"]]
hx = gpd.GeoDataFrame(hx, geometry="geometry", crs=4326).to_crs(CRS)
top = hx.sort_values("n", ascending=False).iloc[0]
c = top.geometry.centroid
print(f"= {len(hx):,} hexagons; busiest has {top['n']:,} records")

# ── map ──────────────────────────────────────────────────────────────────────
countries = fetch.shapes(fetch.COUNTRIES, DATA / "countries.zip")
na = countries[countries["ADM0_A3"].isin(["USA", "MEX", "CAN"])].to_crs(CRS)
states = fetch.shapes(fetch.STATES, DATA / "states.zip").to_crs(CRS)

fig, ax = dmc.figure("square", map_box=(0.04, 0.09, 0.92, 0.70))
na.plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.STONE, lw=0.6)
states.boundary.plot(ax=ax, color=dmc.MIST, lw=0.4)
norm = LogNorm(1, max(10, hx["n"].max()))
hx.plot(ax=ax, column="n", cmap=dmc.SEQ_HEAT, norm=norm, edgecolor=dmc.PARCHMENT, lw=0.25, zorder=3)
x0, y0, x1, y1 = hx.total_bounds
pad = 150_000
cx_, cy_ = (x0 + x1) / 2, (y0 + y1) / 2
half = max(x1 - x0, (y1 - y0) * 0.92 / 0.70) / 2 + pad      # fill the frame's shape
ax.set_xlim(cx_ - half, cx_ + half)
ax.set_ylim(cy_ - half * 0.70 / 0.92, cy_ + half * 0.70 / 0.92)
ax.set_aspect("equal")
gpt = gpd.GeoSeries(gpd.points_from_xy([-116.87], [36.46]), crs=4326).to_crs(CRS).iloc[0]   # Furnace Creek
dmc.label(ax, gpt.x + 60000, gpt.y - 40000, "Death Valley", size=7.5, style="italic", zorder=5)
ax.scatter([gpt.x], [gpt.y], s=10, color=dmc.INK, zorder=5)
dmc.scalebar(ax, 500, loc=(0.78, 0.05))

lx = fig.add_axes((0.08, 0.135, 0.26, 0.016))
grad = np.linspace(0, 1, 256)[None, :]
lx.imshow(grad, aspect="auto", cmap=dmc.SEQ_HEAT, extent=(0, 1, 0, 1))
lx.set_yticks([])
ticks = [v for v in (1, 10, 100, 1000, 10000) if v <= norm.vmax]
lx.set_xticks([np.log(v) / np.log(norm.vmax) for v in ticks])
lx.set_xticklabels([f"{v:,}" for v in ticks], family=dmc.MONO, fontsize=7, color=dmc.STONE)
lx.tick_params(length=2)
for sp in lx.spines.values():
    sp.set_visible(False)
fig.text(0.08, 0.16, "RECORDS PER HEXAGON", family=dmc.MONO, size=6.8, color=dmc.STONE)

dmc.frame(
    fig, DAY,
    subtitle=(f"{len(df):,} kangaroo rat records from GBIF, {df['species'].nunique()} species, binned into hexagons of about\n"
              f"1,770 km². {spec:.0%} are museum specimens; only {inat:.0%} are people's sightings."),
    source=f"GBIF.org, genus Dipodomys ({pd.Timestamp.now():%b %Y}) · Natural Earth · U.S. Census",
    note="Where kangaroo rats were recorded, which is also where people went looking. Uber H3 grid, resolution 4.",
)
dmc.save(fig, DAY, alt=(
    f"Hexagon map of western North America shaded by how many kangaroo rat records each holds, {len(df):,} in all "
    f"across {len(hx):,} hexagons; the busiest holds {top['n']:,}. Death Valley is marked."))
