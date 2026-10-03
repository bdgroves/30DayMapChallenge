"""
Day 3 · Polygons — Every fire around Groveland

Every recorded fire perimeter touching Tuolumne County since 1900, coloured by year and laid
semi-transparent so ground that burned again and again shows darker. The Rim Fire (2013) is
outlined. Counts of how much of the county has burned come from a 100 m grid of the perimeters.

Downloads (cached in data/): CAL FIRE FRAP perimeters (ArcGIS REST), Census county outlines,
Copernicus 90 m DEM.
"""
import io
import json
import sys
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import terrain  # noqa: E402

import geopandas as gpd  # noqa: E402
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.colors import Normalize  # noqa: E402
from rasterio import features  # noqa: E402
from rasterio.transform import from_origin  # noqa: E402

DAY = 3
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
UA = {"User-Agent": "30DayMapChallenge-2026 (github.com/bdgroves/30DayMapChallenge)"}
CRS = "EPSG:32610"
FRAP = "https://services1.arcgis.com/jUJYIo9tSA7EHvfZ/arcgis/rest/services/California_Historic_Fire_Perimeters/FeatureServer/0/query"
FIRST = 1900


def get(url: str, name: str) -> bytes:
    p = DATA / name
    if not p.exists():
        print(f"  downloading {name}")
        with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=300) as r:
            p.write_bytes(r.read())
    return p.read_bytes()


# ── Tuolumne County ──────────────────────────────────────────────────────────
z = zipfile.ZipFile(io.BytesIO(get("https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip",
                                   "counties.zip")))
cdir = DATA / "counties"
if not cdir.exists():
    z.extractall(cdir)
counties = gpd.read_file(next(cdir.glob("*.shp")))
ca = counties[counties["STATEFP"] == "06"].to_crs(CRS)
county = ca[ca["GEOID"] == "06109"]
w, s, e, n = county.to_crs(4326).total_bounds

# ── fire perimeters, paged from the CAL FIRE service ─────────────────────────
perim = DATA / "frap_tuolumne.geojson"
if not perim.exists():
    feats, offset = [], 0
    while True:
        params = dict(where=f"YEAR_ >= {FIRST}", geometry=f"{w},{s},{e},{n}", geometryType="esriGeometryEnvelope",
                      inSR=4326, spatialRel="esriSpatialRelIntersects", outSR=4326, f="geojson",
                      outFields="YEAR_,FIRE_NAME,GIS_ACRES,ALARM_DATE,AGENCY", resultOffset=offset,
                      resultRecordCount=1000, orderByFields="OBJECTID")
        req = urllib.request.Request(FRAP + "?" + urllib.parse.urlencode(params), headers=UA)
        with urllib.request.urlopen(req, timeout=300) as r:
            page = json.loads(r.read())
        got = page.get("features", [])
        feats += got
        print(f"  perimeters: {len(feats)}")
        if len(got) < 1000 and not page.get("exceededTransferLimit"):
            break
        offset += len(got)
    perim.write_text(json.dumps({"type": "FeatureCollection", "features": feats}))
fires = gpd.read_file(perim).to_crs(CRS)
fires = fires[fires.geometry.notna() & fires["YEAR_"].notna()].copy()
fires["YEAR_"] = fires["YEAR_"].astype(int)
fires["geometry"] = fires.geometry.buffer(0)
fires = fires[fires.intersects(county.geometry.iloc[0])].sort_values("YEAR_")
print(f"  {len(fires)} fires touching Tuolumne County, {fires['YEAR_'].min()}–{fires['YEAR_'].max()}")

# ── how much of the county has burned, and how often (100 m grid) ────────────
cx0, cy0, cx1, cy1 = county.total_bounds
res = 100
shape = (int((cy1 - cy0) / res) + 1, int((cx1 - cx0) / res) + 1)
tf = from_origin(cx0, cy1, res, res)
inside = features.rasterize([(county.geometry.iloc[0], 1)], out_shape=shape, transform=tf, dtype="uint8") == 1
times = features.rasterize(((g, 1) for g in fires.geometry), out_shape=shape, transform=tf, dtype="uint16",
                           merge_alg=features.MergeAlg.add)
cells = inside.sum()
burned = (times[inside] >= 1).sum() / cells
twice = (times[inside] >= 2).sum() / cells
most = int(times[inside].max())
print(f"  burned at least once: {burned:.1%}; twice or more: {twice:.1%}; most times: {most}")

rim = fires[fires["FIRE_NAME"].str.upper().str.strip() == "RIM"]
rim = rim[rim["YEAR_"] == 2013]
rim_acres = rim["GIS_ACRES"].sum() if len(rim) else None
largest = fires.sort_values("GIS_ACRES", ascending=False).head(5)

# ── figure ───────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("wide", map_box=(0.03, 0.08, 0.66, 0.72))
pad = 6000
x0, x1, y0, y1 = cx0 - pad, cx1 + pad, cy0 - pad, cy1 + pad
zz, ztf = terrain.dem(tuple(gpd.GeoSeries.from_xy([x0, x1], [y0, y1], crs=CRS).to_crs(4326).total_bounds), res=90)
ax.imshow(terrain.relief(zz, 90, strength=0.55, exaggerate=1.2), extent=terrain.extent(ztf, zz.shape),
          interpolation="bilinear", zorder=0)
ax.set_xlim(x0, x1)
ax.set_ylim(y0, y1)
ax.set_aspect("equal")

# fade everything outside the county
outside = gpd.GeoSeries([gpd.GeoSeries.from_xy([x0], [y0], crs=CRS).iloc[0].buffer(1e6).difference(
    county.geometry.iloc[0])], crs=CRS)
norm = Normalize(FIRST, int(fires["YEAR_"].max()))
fires.plot(ax=ax, color=[dmc.SEQ_HEAT(0.15 + 0.85 * norm(y)) for y in fires["YEAR_"]], alpha=0.42,
           edgecolor="none", zorder=2)
outside.plot(ax=ax, color=dmc.PARCHMENT, alpha=0.72, zorder=3)
county.boundary.plot(ax=ax, color=dmc.INK, lw=0.8, zorder=4)
if len(rim):
    rim.boundary.plot(ax=ax, color=dmc.INK, lw=0.9, linestyle="dashed", zorder=5)
    c = rim.geometry.union_all().representative_point()
    dmc.label(ax, c.x, c.y, f"Rim Fire, 2013\n{rim_acres:,.0f} acres", size=8, weight="bold", ha="center",
              va="center", zorder=6)

from pyproj import Transformer  # noqa: E402
to = Transformer.from_crs(4326, CRS, always_xy=True)
for name, (lon, lat), dx in [("Groveland", (-120.2313, 37.8385), 1200), ("Sonora", (-120.3822, 37.9841), 1200),
                             ("Tuolumne", (-120.2377, 37.9616), 1200), ("Twain Harte", (-120.2316, 38.0396), 1200),
                             ("Pinecrest", (-119.9988, 38.1913), 1200), ("Hetch Hetchy", (-119.7856, 37.9474), 1200),
                             ("Jamestown", (-120.4227, 37.9530), -1200)]:
    x, y = to.transform(lon, lat)
    ax.scatter([x], [y], s=12 if name == "Groveland" else 6, color=dmc.INK, zorder=6)
    dmc.label(ax, x + dx, y, name, size=8.5 if name == "Groveland" else 7, ha="left" if dx > 0 else "right",
              va="center", weight="bold" if name == "Groveland" else "normal", zorder=6)
dmc.scalebar(ax, 20, loc=(0.04, 0.05))

# side panel: numbers, legend, largest fires
px = 0.715
fig.text(px, 0.79, f"{burned:.0%}", family=dmc.TITLE, weight=900, size=38, color=dmc.LAVA, va="top")
fig.text(px, 0.675, f"of Tuolumne County has burned at least once\nsince {FIRST}, and {twice:.0%} has burned twice or more.\n"
         f"The most-burned ground has burned {most} times.", size=8.5, va="top", linespacing=1.55)
lx = fig.add_axes((px, 0.47, 0.25, 0.03))
grad = np.linspace(0, 1, 256)[None, :]
lx.imshow(grad, aspect="auto", cmap=dmc.SEQ_HEAT, alpha=0.8, extent=(FIRST, norm.vmax, 0, 1), vmin=-0.18, vmax=1)
lx.set_yticks([])
lx.set_xticks([1900, 1950, 2000, norm.vmax])
lx.tick_params(labelsize=7, length=2, colors=dmc.STONE)
for t in lx.get_xticklabels():
    t.set_fontfamily(dmc.MONO)
for sp in lx.spines.values():
    sp.set_visible(False)
fig.text(px, 0.515, "YEAR OF THE FIRE  ·  OVERLAPS SHOW DARKER", family=dmc.MONO, size=6.8, color=dmc.STONE)
fig.text(px, 0.385, "LARGEST FIRES IN THE RECORD", family=dmc.MONO, size=6.8, color=dmc.STONE)
for i, (_, f) in enumerate(largest.iterrows()):
    y = 0.36 - i * 0.034
    fig.text(px, y, f"{f['YEAR_']}", family=dmc.MONO, size=8, color=dmc.LAVA, va="center")
    fig.text(px + 0.045, y, str(f["FIRE_NAME"]).title(), size=8.5, va="center")
    fig.text(0.965, y, f"{f['GIS_ACRES']:,.0f} ac", family=dmc.MONO, size=7.5, color=dmc.STONE, va="center", ha="right")

dmc.frame(
    fig, DAY,
    subtitle=(f"{len(fires):,} recorded fire perimeters touching Tuolumne County since {FIRST}, oldest pale, newest red.\n"
              f"Where they overlap, the colour deepens. Groveland sits at the western edge of the Rim Fire."),
    source="CAL FIRE FRAP fire perimeters (April 2026) · U.S. Census Bureau · Copernicus DEM GLO-90",
    note="Older records are incomplete: some early fires were never mapped, so the oldest decades are undercounted.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Tuolumne County, California, covered in overlapping semi-transparent fire perimeters from {FIRST} to "
    f"{norm.vmax}, pale for old fires and red for recent ones. The 2013 Rim Fire, outlined with a dashed line, fills "
    f"much of the middle of the county east of Groveland. {burned:.0%} of the county has burned at least once and "
    f"{twice:.0%} twice or more."))
