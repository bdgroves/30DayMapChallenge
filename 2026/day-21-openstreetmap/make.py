"""
Day 21 · OpenStreetMap — Groveland, by volunteers

Everything OpenStreetMap holds for Groveland and Big Oak Flat, the two old gold-rush towns on
Highway 120 where I grew up, drawn straight from the raw data with nothing added: every
building, road, path, stream and named place volunteers have mapped. The map is a draft of
"before": the plan is to spend an October evening adding what's missing, then draw it again.

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

name = "groveland_wide.json"
if REFRESH and (DATA / name).exists():
    (DATA / name).unlink()
q = f"[out:json][timeout:180];nwr({','.join(map(str, BBOX))});out geom;"
js = json.loads(fetch.get(OVERPASS + "?" + urllib.parse.urlencode({"data": q}), DATA / name, timeout=300))
els = js["elements"]
stamp = js.get("osm3s", {}).get("timestamp_osm_base", "")[:10]

polys, lines, points = [], [], []
for e in els:
    t = e.get("tags", {})
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

# ── map ──────────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("wide", map_box=(0.03, 0.08, 0.94, 0.72))
GREEN = {"forest", "wood", "grass", "meadow", "scrub", "park", "golf_course", "recreation_ground", "pitch", "cemetery"}


def fill(r):
    kind = r.get("landuse") or r.get("leisure") or r.get("natural") or r.get("amenity")
    if kind in GREEN:
        return "#dde3cf"
    if kind == "water":
        return "#cfe0e6"
    if kind == "parking":
        return "#e6e0d4"
    return dmc.CREAM


if len(land):
    land.plot(ax=ax, color=[fill(r) for _, r in land.iterrows()], edgecolor=dmc.MIST, lw=0.2, zorder=1)
W = {"motorway": 2.4, "trunk": 2.2, "primary": 2.2, "secondary": 1.6, "tertiary": 1.3, "residential": 0.8,
     "unclassified": 0.8, "service": 0.45, "track": 0.45, "path": 0.35, "footway": 0.35}
for kind, g in roads.groupby(col(roads, "highway")):
    w = W.get(kind, 0.4)
    dashed = kind in ("track", "path", "footway", "bridleway", "cycleway")
    g.plot(ax=ax, color=dmc.STONE if dashed else dmc.INK, lw=w, ls="dashed" if dashed else "solid",
           alpha=0.85, zorder=3 if w > 1 else 2)
water.plot(ax=ax, color=dmc.LAKE, lw=0.6, alpha=0.8, zorder=2)
bld.plot(ax=ax, color=dmc.LAVA, lw=0, zorder=4)
named.plot(ax=ax, color=dmc.GOLD, markersize=10, edgecolor=dmc.INK, lw=0.4, zorder=5)
for nm, lon, lat in [("GROVELAND", -120.2305, 37.8385), ("BIG OAK FLAT", -120.2585, 37.8235)]:
    x, y = gpd.GeoSeries([Point(lon, lat)], crs=4326).to_crs(CRS).iloc[0].coords[0]
    ax.text(x, y + 380, nm, family=dmc.MONO, size=10, color=dmc.INK, alpha=0.55, ha="center", zorder=7)
b = gpd.GeoSeries([Point(BBOX[1], BBOX[0]), Point(BBOX[3], BBOX[2])], crs=4326).to_crs(CRS)
ax.set_xlim(b.iloc[0].x, b.iloc[1].x)
ax.set_ylim(b.iloc[0].y, b.iloc[1].y)
ax.set_aspect("equal")
ax.apply_aspect()
# label what fits without overlapping: schools, museums and civic places first, then the rest
rank = lambda r: 0 if (r.get("amenity") in ("school", "library", "townhall", "post_office", "fire_station")  # noqa: E731
                       or r.get("tourism") == "museum" or r.get("historic")) else 1
items = sorted(((rank(r), r["name"], r.geometry.x, r.geometry.y) for _, r in named.iterrows()))
placed = dmc.place_labels(ax, [(x, y, n) for _, n, x, y in items], size=5.6, zorder=6)
print(f"  labelled {len(placed)} of {len(named)} named places")
dmc.scalebar(ax, 1, loc=(0.03, 0.05))
from matplotlib.lines import Line2D  # noqa: E402
ax.legend(handles=[Line2D([], [], marker="s", ls="", color=dmc.LAVA, label=f"{len(bld):,} buildings"),
                   Line2D([], [], color=dmc.INK, lw=1.4, label=f"{road_km:.0f} km of road and path"),
                   Line2D([], [], marker="o", ls="", color=dmc.GOLD, mec=dmc.INK, label=f"{len(named)} named places")],
          loc="lower right", fontsize=7.5, frameon=True, facecolor=dmc.PARCHMENT, edgecolor=dmc.MIST, framealpha=0.92)

dmc.frame(
    fig, DAY,
    subtitle=(f"Everything OpenStreetMap's volunteers have mapped in the two gold-rush towns on Highway 120 where I grew up:\n"
              f"{len(bld):,} buildings, {road_km:.0f} km of road and path, {len(named)} named places. Before I add what's missing."),
    source=f"© OpenStreetMap contributors (ODbL), via the Overpass API, data as of {stamp}",
    note="Drawn from the raw OSM data with nothing added: what isn't here hasn't been mapped yet.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Groveland and Big Oak Flat, California, drawn only from OpenStreetMap: {len(bld):,} buildings in red, "
    f"roads in black, paths dashed, streams in blue and {len(named)} named places in gold along Highway 120."))
