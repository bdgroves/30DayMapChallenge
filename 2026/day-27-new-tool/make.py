"""
Day 27 · New tool — Every building in Washington

Every building footprint Overture Maps holds for Washington, counted into a fine grid by DuckDB
straight from Overture's GeoParquet on S3: no download of the footprints, just one SQL query
whose filters DuckDB pushes down to the files. Both are new to me. The grid is drawn here as a
still; lonboard, the other new tool on the plan, comes next for the interactive version.

The query also asks where each footprint came from (OpenStreetMap volunteers, Microsoft's or
Google's machine-learned footprints, Esri...), the first source Overture lists for it.

Downloads (cached in data/): the gridded counts (data/grid.parquet), Census state outline.
"""
import json
import sys
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import duckdb  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
import shapely  # noqa: E402
from matplotlib.colors import LogNorm  # noqa: E402

DAY = 27
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
BB = (-124.85, 45.54, -116.91, 49.01)                 # Washington's bounding box
DX, DY = 0.006, 0.004                                  # about 450 m x 445 m at 47°N
STAC = "https://stac.overturemaps.org/catalog.json"
FALLBACK = "2026-09-17.0"


def latest_release() -> str:
    try:
        with urllib.request.urlopen(urllib.request.Request(STAC, headers={"User-Agent": "30DMC"}), timeout=60) as r:
            cat = json.loads(r.read())
        rel = sorted(lk["href"].strip("./").split("/")[0] for lk in cat["links"] if lk.get("rel") == "child")
        return rel[-1]
    except Exception as e:  # noqa: BLE001
        print(f"  STAC catalog: {e}; using {FALLBACK}")
        return FALLBACK


grid_p = DATA / "grid.parquet"
meta_p = DATA / "grid.json"
if not grid_p.exists():
    rel = latest_release()
    con = duckdb.connect()
    con.sql("INSTALL httpfs; LOAD httpfs; SET s3_region='us-west-2';")
    src = f"s3://overturemaps-us-west-2/release/{rel}/theme=buildings/type=building/*"
    print(f"  querying Overture {rel}", flush=True)
    df = con.sql(f"""
        SELECT floor(((bbox.xmin + bbox.xmax) / 2 - {BB[0]}) / {DX})::INT AS gx,
               floor(((bbox.ymin + bbox.ymax) / 2 - {BB[1]}) / {DY})::INT AS gy,
               coalesce(sources[1].dataset, 'unknown') AS src,
               count(*) AS n
        FROM read_parquet('{src}', hive_partitioning = 1)
        WHERE bbox.xmin > {BB[0]} AND bbox.xmax < {BB[2]} AND bbox.ymin > {BB[1]} AND bbox.ymax < {BB[3]}
        GROUP BY ALL
    """).df()
    df.to_parquet(grid_p)
    meta_p.write_text(json.dumps({"release": rel}))
df = pd.read_parquet(grid_p)
rel = json.loads(meta_p.read_text())["release"]

# keep cells whose centre is in Washington
wa = fetch.shapes(fetch.STATES, DATA / "states.zip")
wa = wa[wa["STUSPS"] == "WA"].to_crs(4326).geometry.union_all()
cx = BB[0] + (df["gx"] + 0.5) * DX
cy = BB[1] + (df["gy"] + 0.5) * DY
df = df[shapely.contains_xy(wa, cx.to_numpy(), cy.to_numpy())]
total = int(df["n"].sum())
by_src = df.groupby("src")["n"].sum().sort_values(ascending=False)
cells = df.groupby(["gx", "gy"])["n"].sum()
nx, ny = int(np.ceil((BB[2] - BB[0]) / DX)), int(np.ceil((BB[3] - BB[1]) / DY))
img = np.full((ny, nx), np.nan)
gx = cells.index.get_level_values(0).to_numpy()
gy = cells.index.get_level_values(1).to_numpy()
ok = (gx >= 0) & (gx < nx) & (gy >= 0) & (gy < ny)
img[gy[ok], gx[ok]] = cells.to_numpy()[ok]
print(f"= Overture {rel}: {total:,} buildings in Washington in {len(cells):,} cells; by source: "
      + ", ".join(f"{k} {v:,}" for k, v in by_src.items()))

# ── map ──────────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("wide", dark=True, map_box=(0.03, 0.09, 0.72, 0.70))
ax.imshow(img, origin="lower", extent=(BB[0], BB[2], BB[1], BB[3]), cmap="inferno", norm=LogNorm(1, np.nanmax(img)),
          interpolation="nearest", zorder=1)
from shapely.geometry import MultiPolygon  # noqa: E402
for poly in (wa.geoms if isinstance(wa, MultiPolygon) else [wa]):
    if poly.area > 0.01:
        ax.plot(*poly.exterior.xy, color="#3a3e45", lw=0.6, zorder=0)
ax.set_xlim(BB[0], BB[2])
ax.set_ylim(BB[1], BB[3])
ax.set_aspect(1 / np.cos(np.radians(47.3)))
for name, lon, lat in [("Seattle", -122.33, 47.61), ("Spokane", -117.43, 47.66), ("Yakima", -120.51, 46.60),
                       ("Tri-Cities", -119.2, 46.23), ("Bellingham", -122.48, 48.75), ("Wenatchee", -120.31, 47.42),
                       ("Vancouver", -122.67, 45.64)]:
    ax.text(lon + 0.07, lat, name, family=dmc.MONO, size=7, color=dmc.MIST, va="center", zorder=3)

# where the footprints came from
NICE = {"OpenStreetMap": "OpenStreetMap volunteers", "Microsoft ML Buildings": "Microsoft, machine-learned",
        "Google Open Buildings": "Google, machine-learned", "Esri Community Maps": "Esri Community Maps"}
x0, y = 0.78, 0.74
fig.text(x0, y, "WHO DREW THEM", family=dmc.MONO, size=8, color=dmc.WHITE)
y -= 0.05
for k, v in by_src.head(5).items():
    share = v / total
    fig.text(x0, y, NICE.get(k, k), size=8, color=dmc.WHITE)
    fig.text(0.97, y, f"{share:.0%}", family=dmc.MONO, size=8, color=dmc.GOLD, ha="right")
    fig.add_artist(__import__("matplotlib").patches.Rectangle((x0, y - 0.022), 0.19 * share, 0.008, color=dmc.GOLD,
                                                               transform=fig.transFigure))
    y -= 0.065
fig.text(x0, y - 0.01, f"{total:,} footprints", family=dmc.MONO, size=8, color=dmc.MIST)

dmc.frame(
    fig, DAY,
    subtitle=(f"{total:,} building footprints from Overture Maps, counted into half-kilometre cells by one DuckDB query run\n"
              f"against Overture's files on S3. Brighter means more buildings."),
    source=f"Overture Maps Foundation, buildings, release {rel} (ODbL and CDLA) · U.S. Census Bureau",
    note="Each footprint is counted once, in the cell holding the centre of its bounding box.",
)
dmc.save(fig, DAY, alt=(
    f"Dark map of Washington State where each half-kilometre cell glows by how many of Overture Maps' {total:,} building "
    f"footprints it holds, brightest around Puget Sound and Spokane. A panel "
    f"shows who drew the footprints, led by {NICE.get(by_src.index[0], by_src.index[0])} at {by_src.iloc[0] / total:.0%}."))
