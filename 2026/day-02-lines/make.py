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
paths = overpass(f'way["highway"~"^(footway|path|pedestrian|cycleway|steps)$"]({bb});', "paths.json")
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

# ── closing the loop ────────────────────────────────────────────────────────
# The Seawall leaves the park at Coal Harbour and at English Bay. Its two loose ends are the endpoints
# in the south of the park that no other piece of Seawall comes near; the loop closes along the park's
# own footpaths between them, the way walkers come back past Lost Lagoon.
import networkx as nx  # noqa: E402
from pyproj import Transformer  # noqa: E402
to_utm = Transformer.from_crs(4326, UTM, always_xy=True)
Wl = list(W)
south = park_poly.bounds[1] + (park_poly.bounds[3] - park_poly.bounds[1]) * 0.35
ends = []
for i, ln in enumerate(Wl):
    for pt in (Point(ln.coords[0]), Point(ln.coords[-1])):
        others = min((o.distance(pt) for j, o in enumerate(Wl) if j != i), default=1e9)
        lat = gpd.GeoSeries([pt], crs=UTM).to_crs(4326).iloc[0].y
        if others > 30 and lat < south:
            ends.append(pt)
connector = None
if len(ends) >= 2:
    a, b = max(((p, q) for p in ends for q in ends), key=lambda pq: pq[0].distance(pq[1]))
    G = nx.Graph()
    # the park's own paths, and the ones around Lost Lagoon, which OpenStreetMap maps outside the park
    lagoon = [g for el, g in ((el, polys({"elements": [el]})) for el in green.get("elements", []))
              if el.get("tags", {}).get("name") == "Lost Lagoon" for g in g]
    inpark = gpd.GeoSeries([unary_union([park_poly.buffer(0.0002)] + [g.buffer(0.0012) for g in lagoon])],
                           crs=4326).to_crs(UTM).iloc[0]
    print(f"= Lost Lagoon polygons: {len(lagoon)}")
    for el in paths.get("elements", []):
        t = el.get("tags", {})
        if el.get("type") != "way" or "geometry" not in el or "seawall" in (t.get("name") or "").lower():
            continue
        xs, ys = to_utm.transform([q["lon"] for q in el["geometry"]], [q["lat"] for q in el["geometry"]])
        nodes = [(round(x, 1), round(y, 1)) for x, y in zip(xs, ys)]
        for u, v in zip(nodes, nodes[1:]):
            d = ((u[0] - v[0]) ** 2 + (u[1] - v[1]) ** 2) ** 0.5
            inside = inpark.contains(Point((u[0] + v[0]) / 2, (u[1] + v[1]) / 2))
            G.add_edge(u, v, weight=d if inside else d * 8, inside=inside)
    if G.number_of_nodes():
        nodes = np.array(list(G.nodes))
        snap = lambda pt: tuple(nodes[np.argmin((nodes[:, 0] - pt.x) ** 2 + (nodes[:, 1] - pt.y) ** 2)])  # noqa: E731
        na, nb = snap(a), snap(b)
        gap = (Point(na).distance(a), Point(nb).distance(b))
        try:
            route = nx.shortest_path(G, na, nb, weight="weight")
            connector = LineString([(a.x, a.y)] + [tuple(n) for n in route] + [(b.x, b.y)])
            out = sum(G[u][v]["weight"] / 8 for u, v in zip(route, route[1:]) if not G[u][v]["inside"])
            print(f"= closing path: {out:.0f} m of it outside the park and lagoon")
        except nx.NetworkXNoPath:
            pass
    print(f"= loose ends {len(ends)}; closing path "
          + (f"{connector.length / 1000:.2f} km (snapped {gap[0]:.0f} m and {gap[1]:.0f} m)" if connector else "not found"))
loop_km = route_km + (connector.length / 1000 if connector else 0)

# ── map ──────────────────────────────────────────────────────────────────────
MAPBOX = basemap.available()
CRS = "EPSG:3857" if MAPBOX else UTM
fig, ax = dmc.figure("square", map_box=(0.05, 0.095, 0.90, 0.685))
P = gpd.GeoSeries([park_poly], crs=4326).to_crs(CRS)
x0, y0, x1, y1 = P.total_bounds
pad = (x1 - x0) * 0.12
ax.set_xlim(x0 - pad, x1 + pad)
ax.set_ylim(y0 - pad, y1 + pad)
ax.set_aspect("equal")
drawn = MAPBOX and basemap.mapbox(ax, style="mapbox/outdoors-v12")
if not drawn:
    ax.set_facecolor("#cfe0e6")
    P.plot(ax=ax, color=dmc.CREAM, zorder=0)
    G = gpd.GeoSeries(polys(green, lambda t: t.get("natural") == "wood" or t.get("landuse") == "forest"), crs=4326)
    G.to_crs(CRS).plot(ax=ax, color="#cdd8bf", lw=0, zorder=1)
    Wt = gpd.GeoSeries(polys(green, lambda t: t.get("natural") == "water"), crs=4326)
    Wt.to_crs(CRS).plot(ax=ax, color="#a9c4cf", lw=0, zorder=1)
if connector is not None:
    cxs, cys = gpd.GeoSeries([connector], crs=UTM).to_crs(CRS).iloc[0].xy
    ax.plot(cxs, cys, color=dmc.PARCHMENT, lw=4.0, zorder=4, solid_capstyle="round")
    ax.plot(cxs, cys, color=dmc.LAVA, lw=1.8, ls=(0, (2, 1.6)), zorder=5)
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
    subtitle=(f"The path around Stanley Park in Vancouver: {route_km:.1f} km between the forest and the sea,\n"
              + (f"and {connector.length / 1000:.1f} km back past Lost Lagoon to close the loop, {loop_km:.1f} km all the way round."
                 if connector is not None else "walked, run, cycled and skated.")),
    source="OpenStreetMap contributors (Overpass API)" + (" · " + basemap.CREDIT if drawn else ""),
    note="Solid: the Seawall, measured along its cycling route. Dashed: the park's own footpath back to the start.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Stanley Park in Vancouver with the Seawall drawn as a red line around its shore, about {route_km:.1f} km, "
    f"and landmarks including {', '.join(sorted(seen)[:5])}."))
