"""
Day 21 · OpenStreetMap — Groveland, by volunteers

Everything OpenStreetMap holds for Groveland and Big Oak Flat, the two old gold-rush towns on
Highway 120 where I grew up, drawn straight from the raw data with nothing added, and colored by
when a volunteer last touched it. Overpass's "out meta" gives every feature its last-edit date,
its version and the account that made that edit; the map shows the dates, and the panel counts
the people (never their names). Version 1 means nobody has edited it since it was first drawn.
Any edits I make before posting simply show up as 2026.

Downloads (cached in data/): OpenStreetMap via the Overpass API.
"""
import json
import sys
import urllib.parse
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
from shapely.geometry import LineString, Point, Polygon  # noqa: E402

DAY = 21
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
CRS = "EPSG:32610"
BBOX = (37.806, -120.317, 37.856, -120.171)            # S, W, N, E: Big Oak Flat to east Groveland, framed 16:9
OVERPASS = "https://overpass-api.de/api/interpreter"
REFRESH = "--refresh" in sys.argv                       # re-download after editing OSM

name = "groveland_meta.json"
if REFRESH and (DATA / name).exists():
    (DATA / name).unlink()
q = f"[out:json][timeout:180];nwr({','.join(map(str, BBOX))});out meta geom;"
js = json.loads(fetch.get(OVERPASS + "?" + urllib.parse.urlencode({"data": q}), DATA / name, timeout=300))
els = js["elements"]
stamp = js.get("osm3s", {}).get("timestamp_osm_base", "")[:10]

polys, lines, points = [], [], []
for e in els:
    t = {**e.get("tags", {}), "_year": int(e.get("timestamp", "1900")[:4]), "_v": e.get("version", 1)}
    if e["type"] == "node":
        if t:
            points.append({"geometry": Point(e["lon"], e["lat"]), **t})
    elif e["type"] == "way" and "geometry" in e:
        xy = [(p["lon"], p["lat"]) for p in e["geometry"]]
        closed = len(xy) > 3 and xy[0] == xy[-1]
        area = closed and (any(k in t for k in ("building", "landuse", "leisure", "amenity", "natural", "parking"))
                           and t.get("natural") not in ("tree_row", "cliff"))
        if area:
            polys.append({"geometry": Polygon(xy), **t})
        elif len(xy) > 1:
            lines.append({"geometry": LineString(xy), **t})
    elif e["type"] == "relation" and t.get("type") == "multipolygon" and "members" in e:
        for m in e["members"]:
            if m.get("role") == "outer" and m.get("geometry") and len(m["geometry"]) > 3:
                xy = [(p["lon"], p["lat"]) for p in m["geometry"]]
                if xy[0] == xy[-1]:
                    polys.append({"geometry": Polygon(xy), **t})

P = gpd.GeoDataFrame(polys, crs=4326).to_crs(CRS) if polys else gpd.GeoDataFrame(geometry=[], crs=CRS)
L = gpd.GeoDataFrame(lines, crs=4326).to_crs(CRS) if lines else gpd.GeoDataFrame(geometry=[], crs=CRS)
N = gpd.GeoDataFrame(points, crs=4326).to_crs(CRS) if points else gpd.GeoDataFrame(geometry=[], crs=CRS)
col = lambda g, c: g[c] if c in g else gpd.pd.Series([None] * len(g), index=g.index)  # noqa: E731

bld = P[col(P, "building").notna()]
land = P[col(P, "building").isna()]
roads = L[col(L, "highway").notna()]
water = L[col(L, "waterway").notna()]
named = N[col(N, "name").notna() & (col(N, "amenity").notna() | col(N, "shop").notna() | col(N, "tourism").notna()
                                    | col(N, "historic").notna() | col(N, "leisure").notna() | col(N, "office").notna())]
road_km = roads.length.sum() / 1000
kinds = Counter(e["type"] for e in els)
print(f"= OSM {stamp}: {len(els):,} elements ({dict(kinds)}); {len(bld):,} buildings, {road_km:.0f} km of highway=*, "
      f"{len(named)} named places, {len(water)} waterways")

# who and when
feat = [e for e in els if e.get("tags") or e["type"] != "node"]   # untagged nodes are just way vertices
mappers = len({e.get("uid") for e in feat if e.get("uid")})
years = Counter(int(e["timestamp"][:4]) for e in feat if e.get("timestamp"))
v1 = sum(1 for e in feat if e.get("version") == 1) / max(1, len(feat))
y0, y1 = min(years), max(years)
oldest_b = int(bld["_year"].min()) if len(bld) else y0
print(f"= {len(feat):,} features by {mappers} mappers, edited {y0}-{y1}; {v1:.0%} still version 1; "
      f"buildings last touched {dict(sorted(Counter(bld['_year']).items()))}")

# ── map ──────────────────────────────────────────────────────────────────────
import numpy as np  # noqa: E402
from matplotlib.colors import LinearSegmentedColormap, Normalize  # noqa: E402
RAMP = LinearSegmentedColormap.from_list("yr", ["#cfc3a6", "#c8922a", "#c0392b", "#6e1f17", "#2b1a3a"])
NORM = Normalize(y0, y1)
yc = lambda g: RAMP(NORM(g["_year"].to_numpy()))  # noqa: E731
fig, ax = dmc.figure("wide", map_box=(0.03, 0.17, 0.645, 0.56))
GREEN = {"forest", "wood", "grass", "meadow", "scrub", "park", "golf_course", "recreation_ground", "pitch", "cemetery"}


def fill(r):
    kind = r.get("landuse") or r.get("leisure") or r.get("natural") or r.get("amenity")
    if kind in GREEN:
        return "#e4e6d8"
    if kind == "water":
        return "#d8e4e8"
    return "#ebe5d8"


if len(land):
    land.plot(ax=ax, color=[fill(r) for _, r in land.iterrows()], edgecolor=dmc.MIST, lw=0.2, zorder=1)
W = {"motorway": 2.4, "trunk": 2.2, "primary": 2.2, "secondary": 1.6, "tertiary": 1.3, "residential": 0.8,
     "unclassified": 0.8, "service": 0.45, "track": 0.45, "path": 0.35, "footway": 0.35}
for kind, g in roads.groupby(col(roads, "highway")):
    w = W.get(kind, 0.4)
    dashed = kind in ("track", "path", "footway", "bridleway", "cycleway")
    g.plot(ax=ax, color=yc(g), lw=w * 0.75, ls="dashed" if dashed else "solid", zorder=3 if w > 1 else 2)
water.plot(ax=ax, color=dmc.LAKE, lw=0.6, alpha=0.6, zorder=2)
bld.plot(ax=ax, color=yc(bld), edgecolor=yc(bld), lw=0.6, zorder=4)
named.plot(ax=ax, color=yc(named), markersize=12, edgecolor=dmc.INK, lw=0.4, zorder=5)
b = gpd.GeoSeries([Point(BBOX[1], BBOX[0]), Point(BBOX[3], BBOX[2])], crs=4326).to_crs(CRS)
ax.set_xlim(b.iloc[0].x, b.iloc[1].x)
ax.set_ylim(b.iloc[0].y, b.iloc[1].y)
ax.set_aspect("equal")
ax.apply_aspect()
taken = []
towns = [(*gpd.GeoSeries([Point(lon, lat)], crs=4326).to_crs(CRS).iloc[0].coords[0], nm)
         for nm, lon, lat in [("GROVELAND", -120.2305, 37.8395), ("BIG OAK FLAT", -120.2585, 37.8245)]]
dmc.place_labels(ax, towns, gap=0, taken=taken, size=10, family=dmc.MONO, color="#6b6560", halo=dmc.PARCHMENT, zorder=7)
ax.add_patch(__import__("matplotlib").patches.Rectangle((0.012, 0.015), 0.105, 0.11, transform=ax.transAxes,
                                                       color=dmc.PARCHMENT, alpha=0.92, zorder=8))
dmc.scalebar(ax, 1, loc=(0.03, 0.05))
ax.lines[-1].set_zorder(9)
ax.texts[-1].set_zorder(9)

# panel: the color key as a bar chart of features by the year they were last edited
px = 0.70
fig.text(px, 0.775, f"{mappers}", family=dmc.TITLE, weight=900, size=40, color=dmc.LAVA, va="top")
fig.text(px, 0.665, f"volunteers drew this map. {len(feat):,} features,\nlast edited between {y0} and {y1}.\n"
         f"{v1:.0%} haven't been touched since they were drawn.", size=8.5, color=dmc.INK, va="top", linespacing=1.6)
cax = fig.add_axes((px, 0.17, 0.26, 0.32))
ys = np.arange(y0, y1 + 1)
cax.bar(ys, [years.get(y, 0) for y in ys], color=RAMP(NORM(ys)), width=0.8)
for sp in ("top", "right", "left"):
    cax.spines[sp].set_visible(False)
cax.spines["bottom"].set_color(dmc.MIST)
cax.tick_params(labelsize=6.5, colors=dmc.STONE, length=2)
cax.set_yticks([])
cax.set_xticks([t for t in range(y0, y1 + 1) if t % 4 == y1 % 4])
for tl in cax.get_xticklabels():
    tl.set_fontfamily(dmc.MONO)
top = max(years, key=years.get)
cax.text(top, years[top], f"{years[top]:,}", ha="center", va="bottom", family=dmc.MONO, size=6.5, color=dmc.INK)
fig.text(px, 0.515, "FEATURES BY THE YEAR THEY WERE LAST EDITED", family=dmc.MONO, size=6.8, color=dmc.STONE)
fig.text(px, 0.495, "Each is drawn in its year's color on the map.", size=6.8, style="italic", color=dmc.STONE)

dmc.frame(
    fig, DAY,
    subtitle=(f"Everything OpenStreetMap's volunteers have mapped in the two gold-rush towns on Highway 120 where I grew up,\n"
              f"{len(bld):,} buildings and {road_km:.0f} km of road and path, colored by the year someone last touched it."),
    source=f"© OpenStreetMap contributors (ODbL), via the Overpass API, data as of {stamp}",
    note="Last edit, not first drawing: a road fixed last year shows last year. Mappers are counted, never named.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Groveland and Big Oak Flat, California, drawn only from OpenStreetMap: {len(bld):,} buildings, roads, paths "
    f"and named places, each colored by the year it was last edited, from pale tan for {y0} to dark plum for {y1}. "
    f"A panel says {mappers} volunteers drew it, {len(feat):,} features in all, {v1:.0%} untouched since first drawn, "
    f"with a bar chart of features by year last edited; the busiest year is {top}."))
