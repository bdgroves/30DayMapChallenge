"""
Day 2 · Lines — The Seawall

The path around Stanley Park in Vancouver: the Seawall, about nine kilometres of it between the
park and the sea. One line, drawn from OpenStreetMap, with the landmarks along the way.

(make_strava.py is the earlier plan for this day, every Strava activity near home, kept for later.)

Downloads (cached in data/): OpenStreetMap through the Overpass API.
"""
import json
import sys
import urllib.parse
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import basemap  # noqa: E402
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
import numpy as np  # noqa: E402
from shapely.geometry import LineString, Point, Polygon  # noqa: E402
from shapely.ops import linemerge, unary_union  # noqa: E402

DAY = 2
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
BBOX = (49.284, -123.165, 49.318, -123.112)        # S, W, N, E
OVERPASS = "https://overpass-api.de/api/interpreter"
UTM = "EPSG:32610"
LANDMARKS = ["Siwash Rock", "Prospect Point", "Brockton Point Lighthouse", "Third Beach", "Second Beach",
             "Lumberman's Arch", "Beaver Lake", "Girl in a Wetsuit", "Nine O'Clock Gun"]


def overpass(q, name):
    url = OVERPASS + "?" + urllib.parse.urlencode({"data": f"[out:json][timeout:180];{q}out geom;"})
    return json.loads(fetch.get(url, DATA / name, timeout=300))


bb = ",".join(map(str, BBOX))
wall = overpass(f'way["highway"]["name"~"seawall",i]({bb});', "seawall.json")
park = overpass(f'nwr["leisure"="park"]["name"="Stanley Park"]({bb});', "park.json")
names = "|".join(n.replace("'", ".") for n in LANDMARKS)
marks = overpass(f'nwr["name"~"^({names})$"]({bb});', "landmarks2.json")
green = overpass(f'(way["natural"="wood"]({bb});way["landuse"="forest"]({bb});'
                 f'way["natural"="water"]({bb});relation["natural"="water"]({bb}););', "green.json")


def lines(js, pred=lambda t: True):
    out = []
    for el in js.get("elements", []):
        if el.get("type") == "way" and "geometry" in el and pred(el.get("tags", {})):
            out.append(LineString([(p["lon"], p["lat"]) for p in el["geometry"]]))
    return out


def polys(js, pred=lambda t: True):
    out = []
    for el in js.get("elements", []):
        t = el.get("tags", {})
        if not pred(t):
            continue
        if el.get("type") == "way" and "geometry" in el and len(el["geometry"]) > 3:
            out.append(Polygon([(p["lon"], p["lat"]) for p in el["geometry"]]).buffer(0))
        elif el.get("type") == "relation":
            rings = [LineString([(p["lon"], p["lat"]) for p in m["geometry"]]) for m in el.get("members", [])
                     if m.get("role") == "outer" and "geometry" in m]
            merged = linemerge(rings)
            for g in getattr(merged, "geoms", [merged]):
                if g.is_ring:
                    out.append(Polygon(g.coords).buffer(0))
    return out


park_poly = unary_union(polys(park))
segs = lines(wall)
print(f"= {len(segs)} OSM ways named Seawall; park polygon area {gpd.GeoSeries([park_poly], crs=4326).to_crs(UTM).area.iloc[0] / 1e6:.2f} km²")
# keep the Seawall around the park, not the stretches beyond it
near = park_poly.buffer(0.0006)                         # about 45 m: the shore path, not beyond the park
segs = [g for s in segs for g in getattr(s.intersection(near), "geoms", [s.intersection(near)])
        if g.geom_type == "LineString" and g.length > 0]
W = gpd.GeoSeries(segs, crs=4326).to_crs(UTM)
total_ways_km = W.length.sum() / 1000
# a 25 m band around every path, divided by its width: a check on the route length
band = unary_union(W.buffer(12.5))
band_km = band.area / 25 / 1000
# the walking and cycling routes measured separately, from the ways' own tags
def km_of(pred):
    ls = []
    for el in wall.get("elements", []):
        t = el.get("tags", {})
        if el.get("type") == "way" and "geometry" in el and pred(t):
            g = LineString([(p["lon"], p["lat"]) for p in el["geometry"]]).intersection(near)
            ls += [x for x in getattr(g, "geoms", [g]) if x.geom_type == "LineString"]
    return gpd.GeoSeries(ls, crs=4326).to_crs(UTM).length.sum() / 1000 if ls else 0


foot_km = km_of(lambda t: t.get("highway") in ("footway", "pedestrian", "path") and t.get("bicycle") not in ("designated",))
bike_km = km_of(lambda t: t.get("highway") == "cycleway" or t.get("bicycle") == "designated")
print(f"= walking ways {foot_km:.2f} km, cycling ways {bike_km:.2f} km, 25 m band {band_km:.2f} km")
route_km = bike_km                                      # the cycling path goes all the way round
print(f"= {len(segs)} pieces in the park: {total_ways_km:.2f} km of path, the route about {route_km:.2f} km")

# ── map ──────────────────────────────────────────────────────────────────────
MAPBOX = basemap.available()
CRS = "EPSG:3857" if MAPBOX else UTM
fig, ax = dmc.figure("square", map_box=(0.05, 0.09, 0.90, 0.72))
P = gpd.GeoSeries([park_poly], crs=4326).to_crs(CRS)
x0, y0, x1, y1 = P.total_bounds
pad = (x1 - x0) * 0.12
ax.set_xlim(x0 - pad, x1 + pad)
ax.set_ylim(y0 - pad, y1 + pad)
ax.set_aspect("equal")
drawn = MAPBOX and basemap.mapbox(ax)
if not drawn:
    ax.set_facecolor("#cfe0e6")
    P.plot(ax=ax, color=dmc.CREAM, zorder=0)
    G = gpd.GeoSeries(polys(green, lambda t: t.get("natural") == "wood" or t.get("landuse") == "forest"), crs=4326)
    G.to_crs(CRS).plot(ax=ax, color="#cdd8bf", lw=0, zorder=1)
    Wt = gpd.GeoSeries(polys(green, lambda t: t.get("natural") == "water"), crs=4326)
    Wt.to_crs(CRS).plot(ax=ax, color="#a9c4cf", lw=0, zorder=1)
gpd.GeoSeries(segs, crs=4326).to_crs(CRS).plot(ax=ax, color=dmc.PARCHMENT, lw=5.2, zorder=4, capstyle="round")
gpd.GeoSeries(segs, crs=4326).to_crs(CRS).plot(ax=ax, color=dmc.LAVA, lw=2.4, zorder=5, capstyle="round")

# landmarks
seen = set()
for el in marks.get("elements", []):
    n = el.get("tags", {}).get("name")
    if not n or n in seen:
        continue
    c = el.get("center") or (el["geometry"][len(el["geometry"]) // 2] if "geometry" in el else {"lat": el.get("lat"), "lon": el.get("lon")})
    if not c or c.get("lat") is None:
        continue
    seen.add(n)
    p = gpd.GeoSeries([Point(c["lon"], c["lat"])], crs=4326).to_crs(CRS).iloc[0]
    ax.scatter([p.x], [p.y], s=8, color=dmc.INK, zorder=6)
    east = p.x > (x0 + x1) / 2 + (x1 - x0) * 0.2            # labels on the east side go to the left
    dx = (x1 - x0) * 0.014
    dmc.label(ax, p.x - dx if east else p.x + dx, p.y, n, size=7, style="italic", va="center",
              ha="right" if east else "left", zorder=8)
print(f"= landmarks: {sorted(seen)}")
k_m = 1000 / np.cos(np.radians(49.3)) if CRS == "EPSG:3857" else 1000
dmc.scalebar(ax, 1, loc=(0.05, 0.05), crs_units_per_km=k_m)

dmc.frame(
    fig, DAY,
    subtitle=(f"The path around Stanley Park in Vancouver, about {route_km:.1f} km between the forest and the sea.\n"
              f"Walked, run, cycled and skated, all the way round."),
    source="OpenStreetMap contributors (Overpass API)" + (" · " + basemap.CREDIT if drawn else ""),
    note="Where walkers and cyclists have separate paths, both are drawn; the length is the cycling route, which goes all the way round.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Stanley Park in Vancouver with the Seawall drawn as a red line around its shore, about {route_km:.1f} km, "
    f"and landmarks including {', '.join(sorted(seen)[:5])}."))
