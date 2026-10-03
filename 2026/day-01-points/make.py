"""
Day 1 · Points — every geocache I've found

One dot per cache, coloured by the year I found it. The main map is where most of them are; a small
world map below has every find, so the ones in Australia, Iceland and the rest are on the map too.

Data: finds.csv, made from my Geocaching "My Finds" pocket query: position rounded to 0.01° (about a
kilometre), the date of my own "Found it" log, cache type, state and country. The pocket query itself
(the GPX, with everyone's logs and exact coordinates) stays out of the repository: Geocaching's terms
limit sharing it. To rebuild finds.csv from a new pocket query, put the .zip or .gpx in data/ and run
this with --from-gpx.

Downloads (cached in data/): Natural Earth countries and states.
"""
import csv
import sys
import xml.etree.ElementTree as ET
import zipfile
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import basemap  # noqa: E402
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.colors import Normalize  # noqa: E402

DAY = 1
FINDER = "Hipparchus"
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
FINDS = HERE / "finds.csv"
FIND_TYPES = {"Found it", "Attended", "Webcam Photo Taken"}
NOWHERE = "Locationless (Reverse) Cache"           # listed coordinates aren't a real place


# ── rebuilding finds.csv from a pocket query ─────────────────────────────────
def gpx_files():
    for p in sorted(DATA.glob("*.zip")):
        if p.name.startswith("ne_") or p.name in ("countries.zip", "states.zip"):
            continue
        with zipfile.ZipFile(p) as z:
            for n in z.namelist():
                if n.endswith(".gpx") and "-wpts" not in n:
                    yield z.read(n)
    for p in sorted(DATA.glob("*.gpx")):
        if "-wpts" not in p.name:
            yield p.read_bytes()


def parse(raw: bytes):
    out = []
    for w in ET.fromstring(raw).iter():
        if w.tag.split("}")[-1] != "wpt":
            continue
        rec = {"lat": float(w.get("lat")), "lon": float(w.get("lon"))}
        for c in w:
            t = c.tag.split("}")[-1]
            if t == "name":
                rec["code"] = c.text
            elif t == "cache":
                for cc in c:
                    tt = cc.tag.split("}")[-1]
                    if tt in ("type", "state", "country"):
                        rec[tt] = cc.text if cc.text not in (None, "None") else ""
                    elif tt == "logs":
                        mine = first = None
                        for log in cc:
                            f = {x.tag.split("}")[-1]: x for x in log}
                            kind = f["type"].text if f.get("type") is not None else ""
                            if kind not in FIND_TYPES:
                                continue
                            date = f["date"].text if f.get("date") is not None else None
                            first = first or date
                            if f.get("finder") is not None and (f["finder"].text or "").lower() == FINDER.lower():
                                mine = date
                        rec["found"] = (mine or first or "")[:10]
        if rec.get("code", "").startswith("GC"):
            out.append(rec)
    return out


if "--from-gpx" in sys.argv:
    rows = {r["code"]: r for raw in gpx_files() for r in parse(raw)}
    if not rows:
        sys.exit("No pocket query in data/.")
    with open(FINDS, "w", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["lat", "lon", "found", "type", "state", "country"])
        for r in sorted(rows.values(), key=lambda r: r.get("found", "")):
            w.writerow([f"{r['lat']:.2f}", f"{r['lon']:.2f}", r.get("found", ""), r.get("type", ""),
                        r.get("state", ""), r.get("country", "")])
    print(f"= wrote {len(rows)} finds to finds.csv")

# ── the finds ────────────────────────────────────────────────────────────────
df = pd.read_csv(FINDS, dtype={"state": str, "country": str}).fillna({"state": "", "country": ""})
df["found"] = pd.to_datetime(df["found"], errors="coerce")
df = df.dropna(subset=["found"])
df["year"] = df["found"].dt.year
total = len(df)
nowhere = int((df["type"] == NOWHERE).sum())
df = df[df["type"] != NOWHERE]
df["where"] = np.where(df["country"] == "United States", df["state"], df["country"])
g = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df["lon"], df["lat"]), crs=4326)
places = Counter(df["where"].replace("", "Elsewhere"))
n_places = len([k for k in places if k != "Elsewhere"])
n_countries = df["country"].nunique()
print(f"= {total} finds ({nowhere} locationless), {g['found'].min():%Y-%m-%d} to {g['found'].max():%Y-%m-%d}; "
      f"{n_places} states and countries, {n_countries} countries")

countries = fetch.shapes(fetch.COUNTRIES, DATA / "countries.zip")
states = fetch.shapes(fetch.STATES, DATA / "states.zip")

# ── main map: where most of them are ────────────────────────────────────────
lo_lon, hi_lon = np.percentile(g["lon"], [5, 95])
lo_lat, hi_lat = np.percentile(g["lat"], [5, 95])
box = (lo_lon - 1.2, lo_lat - 0.8, hi_lon + 1.2, hi_lat + 0.8)
lon0, lat0 = (box[0] + box[2]) / 2, (box[1] + box[3]) / 2
MAPBOX = basemap.available()                       # Mapbox basemaps are Web Mercator
CRS = "EPSG:3857" if MAPBOX else f"+proj=aea +lat_1={box[1] + 1} +lat_2={box[3] - 1} +lat_0={lat0} +lon_0={lon0} +datum=WGS84 +units=m"
inside = g.cx[box[0]:box[2], box[1]:box[3]]
print(f"= main map {box[0]:.1f},{box[1]:.1f} to {box[2]:.1f},{box[3]:.1f}: {len(inside)} finds")

MAP_BOX = (0.05, 0.315, 0.90, 0.535)
fig, ax = dmc.figure("portrait", map_box=MAP_BOX)
norm = Normalize(g["year"].min(), g["year"].max())
col = lambda y: dmc.SEQ_HEAT(0.28 + 0.72 * norm(y))  # noqa: E731
gp = g.to_crs(CRS).sort_values("found")
ax.scatter(gp.geometry.x, gp.geometry.y, s=8, c=[col(y) for y in gp["year"]], edgecolor=dmc.INK, lw=0.18,
           alpha=0.92, zorder=3)
bb = gpd.GeoSeries.from_xy([box[0], box[2]], [box[1], box[3]], crs=4326).to_crs(CRS).total_bounds
want = (MAP_BOX[2] * 8) / (MAP_BOX[3] * 10)          # the frame's shape, so the map fills it
cx, cy, bw, bh = (bb[0] + bb[2]) / 2, (bb[1] + bb[3]) / 2, bb[2] - bb[0], bb[3] - bb[1]
bw, bh = (bh * want, bh) if bw / bh < want else (bw, bw / want)
ax.set_xlim(cx - bw / 2, cx + bw / 2)
ax.set_ylim(cy - bh / 2, cy + bh / 2)
ax.set_aspect("equal")
if not (MAPBOX and basemap.mapbox(ax)):
    MAPBOX = False
    ax.set_facecolor("#dfe8ea")
    countries.to_crs(CRS).plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5, zorder=0)
    states[states["admin"].isin(["United States of America", "Canada", "Mexico"])].to_crs(CRS).boundary.plot(
        ax=ax, color=dmc.MIST, lw=0.5, zorder=1)
for sp in ax.spines.values():
    sp.set_visible(True)
    sp.set_color(dmc.MIST)
    sp.set_linewidth(0.6)

# ── the world: every find ───────────────────────────────────────────────────
WORLD = "+proj=eqearth +lon_0=-150 +datum=WGS84 +units=m"     # Pacific-centred: home, Australia, NZ together
wax = fig.add_axes((0.05, 0.095, 0.47, 0.185))
wax.set_axis_off()
land = countries[countries["ADM0_A3"] != "ATA"].copy()
land["geometry"] = land.geometry.buffer(0)
from shapely.geometry import box as sbox  # noqa: E402
cut = gpd.GeoDataFrame(geometry=[sbox(-180, -90, -150 + 180 - 0.5, 90), sbox(-150 + 180 + 0.5, -90, 180, 90)], crs=4326)
land = gpd.overlay(land[["geometry"]], cut, how="intersection")   # split at the antimeridian of lon_0
land.to_crs(WORLD).plot(ax=wax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.3)
gw = g.to_crs(WORLD).sort_values("found")
wax.scatter(gw.geometry.x, gw.geometry.y, s=3, c=[col(y) for y in gw["year"]], edgecolor=dmc.INK, lw=0.1, zorder=3)
x0, y0, x1, y1 = gw.total_bounds
pad = 1_200_000
wax.set_xlim(x0 - pad, x1 + pad)
wax.set_ylim(min(y0 - pad, -6_500_000), y1 + pad)
wax.set_aspect("equal")
fig.text(0.05, 0.288, f"EVERY FIND · {n_countries} COUNTRIES", family=dmc.MONO, size=6.8, color=dmc.STONE)

# ── finds per year and where ────────────────────────────────────────────────
yax = fig.add_axes((0.585, 0.205, 0.365, 0.07))
per = g.groupby("year").size().reindex(range(g["year"].min(), g["year"].max() + 1), fill_value=0)
yax.bar(per.index, per.values, color=[col(y) for y in per.index], width=0.8)
for sp in ("top", "right", "left"):
    yax.spines[sp].set_visible(False)
yax.spines["bottom"].set_color(dmc.MIST)
yax.set_yticks([])
yax.set_xticks([y for y in per.index if y % 5 == 0])
yax.tick_params(labelsize=6.5, colors=dmc.STONE, length=2)
for t in yax.get_xticklabels():
    t.set_fontfamily(dmc.MONO)
best = int(per.idxmax())
fig.text(0.585, 0.288, f"FINDS PER YEAR · MOST IN {best}: {per.max()}", family=dmc.MONO, size=6.8, color=dmc.STONE)
fig.text(0.585, 0.172, "MOST FINDS", family=dmc.MONO, size=6.8, color=dmc.STONE)
for i, (k, c) in enumerate(places.most_common(5)):
    y = 0.152 - i * 0.0155
    fig.text(0.585, y, k, size=8, va="center")
    fig.text(0.95, y, f"{c:,}", family=dmc.MONO, size=7.5, color=dmc.LAVA, ha="right", va="center")

types = Counter(df["type"])
trad = types.get("Traditional Cache", 0) / len(df)
first, last = g["found"].min(), g["found"].max()
dmc.frame(
    fig, DAY, title=f"{total:,} finds",
    subtitle=(f"Every geocache I've found as {FINDER}, from {first:%B %Y} to {last:%B %Y}, one dot each,\n"
              f"coloured by the year I found it: {n_places} states and countries, {n_countries} countries."),
    source="Geocaching.com 'My Finds' pocket query · Natural Earth" + (" · " + basemap.CREDIT if MAPBOX else ""),
    note=(f"{trad:.0%} traditional caches. Positions rounded to about a kilometre"
          + (f"; {nowhere} locationless caches have no place to draw." if nowhere else ".")),
)
dmc.save(fig, DAY, alt=(
    f"Map of {total:,} geocaches found by {FINDER} from {first:%Y} to {last:%Y}, as small dots coloured from pale gold "
    f"for early years to dark red for recent ones. The main map covers the West Coast, where most are, thickest in "
    f"{', '.join(k for k, _ in places.most_common(3))}. A small world map below shows finds in {n_countries} countries, "
    f"and a bar chart shows finds per year, with the most in {best}."))
