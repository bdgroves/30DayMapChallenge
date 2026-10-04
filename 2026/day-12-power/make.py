"""
Day 12 · Power — The Tuolumne powers San Francisco

San Francisco owns a river. The Hetch Hetchy system dams the Tuolumne in Yosemite, sends the
water 167 miles west by gravity to the city's taps, and on the way drops it through three
powerhouses (Kirkwood, Holm and Moccasin, just below Groveland) whose lines carry the power
to the Bay Area. This map draws the city's own lines and pipes, from OpenStreetMap, over every
other high-voltage line in the region for context.

Basemap: Mapbox Light (quiet, so the lines carry the map); without a token, the house shaded relief.

Downloads (cached in data/): OpenStreetMap via the Overpass API; Copernicus 90 m DEM (fallback only).
"""
import json
import sys
import urllib.parse
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402
import basemap  # noqa: E402
import terrain  # noqa: E402

import numpy as np  # noqa: E402
from matplotlib.collections import LineCollection  # noqa: E402
from pyproj import Transformer  # noqa: E402

DAY = 12
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
MAPBOX = basemap.available()                      # Mapbox basemaps are Web Mercator
CRS = "EPSG:3857" if MAPBOX else "EPSG:32610"
BBOX = (37.2, -122.65, 38.25, -119.3)             # S, W, N, E for Overpass
OVERPASS = "https://overpass-api.de/api/interpreter"
SF = r"San Francisco|SFPUC|Hetch"


def overpass(q, name):
    url = OVERPASS + "?" + urllib.parse.urlencode({"data": f"[out:json][timeout:240];{q}out geom;"})
    return json.loads(fetch.get(url, DATA / name, timeout=400))


bb = ",".join(map(str, BBOX))
hv = overpass(f'way["power"="line"]["voltage"~"^(115|230|500)"]({bb});', "hv_lines.json")
sf = overpass(f'(way["power"~"line|minor_line"]["operator"~"{SF}",i]({bb});'
              f'way["man_made"="pipeline"]["name"~"Hetch",i]({bb});'
              f'way["waterway"~"canal|pipeline"]["name"~"Hetch",i]({bb});'
              f'way["man_made"="pipeline"]["operator"~"{SF}",i]({bb}););', "sfpuc.json")
plants = overpass(f'(nwr["power"="plant"]["name"~"Moccasin|Kirkwood|Holm|Dion R",i]({bb}););', "plants.json")

to = Transformer.from_crs(4326, CRS, always_xy=True)


def segs(js, pred=lambda t: True):
    out = []
    for el in js.get("elements", []):
        if el.get("type") == "way" and "geometry" in el and pred(el.get("tags", {})):
            xs, ys = to.transform([p["lon"] for p in el["geometry"]], [p["lat"] for p in el["geometry"]])
            out.append(list(zip(xs, ys)))
    return out


hv_s = segs(hv)
sf_power = segs(sf, lambda t: "power" in t)
sf_water = segs(sf, lambda t: "power" not in t)
km = lambda ss: sum(sum(((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2) ** 0.5 for a, b in zip(s, s[1:])) for s in ss) / 1000  # noqa: E731
pl = []
for el in plants.get("elements", []):
    c = el.get("center") or (el["geometry"][0] if "geometry" in el else {"lat": el.get("lat"), "lon": el.get("lon")})
    if c and c.get("lat"):
        pl.append((el.get("tags", {}).get("name", "?"), *to.transform(c["lon"], c["lat"])))
print(f"= {len(hv_s):,} regional HV lines ({km(hv_s):,.0f} km); SFPUC power {len(sf_power)} ways ({km(sf_power):,.0f} km), "
      f"water {len(sf_water)} ways ({km(sf_water):,.0f} km); plants: {[p[0] for p in pl]}")

# ── map ──────────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("wide", map_box=(0.03, 0.08, 0.94, 0.72))
x0, y0 = to.transform(BBOX[1], BBOX[0])
x1, y1 = to.transform(BBOX[3], BBOX[2])
ax.set_xlim(x0, x1)
ax.set_ylim(y0, y1)
ax.set_aspect("equal")
drawn = MAPBOX and basemap.mapbox(ax, style="mapbox/light-v11")
if not drawn:
    z, tf = terrain.dem((BBOX[1] - 0.25, BBOX[0] - 0.25, BBOX[3] + 0.25, BBOX[2] + 0.25), crs=CRS, res=180, src_res=90)
    ax.imshow(terrain.relief(z, 180, strength=0.45, exaggerate=1.5, water="#dfe6e3"), extent=terrain.extent(tf, z.shape),
              interpolation="bilinear", zorder=0)
ax.add_collection(LineCollection(hv_s, colors=dmc.STONE, linewidths=0.35, alpha=0.6, zorder=1))
ax.add_collection(LineCollection(sf_water, colors=dmc.LAKE, linewidths=1.6, zorder=2))
ax.add_collection(LineCollection(sf_power, colors=dmc.GOLD, linewidths=1.8, zorder=3))
for name, x, y in pl:
    ax.scatter([x], [y], s=40, marker="s", color=dmc.LAVA, edgecolor=dmc.PARCHMENT, lw=0.8, zorder=5)
    nm = name.replace(" Powerhouse", "").replace(" Power House", "")
    dx, dy, ha = {"Holm": (-2500, 2500, "right"), "Kirkwood": (2500, -3500, "left")}.get(nm.split()[0], (2500, -3500, "left"))
    dmc.label(ax, x + dx, y + dy, nm, size=7.5, ha=ha, zorder=6)
for name, (lon, lat), ha in [("San Francisco", (-122.42, 37.77), "left"), ("Hetch Hetchy", (-119.75, 37.95), "left"),
                             ("Groveland", (-120.231, 37.838), "left"), ("Modesto", (-120.997, 37.639), "left"),
                             ("Newark", (-122.04, 37.53), "right"), ("Oakdale", (-120.847, 37.767), "left")]:
    x, y = to.transform(lon, lat)
    ax.scatter([x], [y], s=10, color=dmc.INK, zorder=6)
    dmc.label(ax, x + (3000 if ha == "left" else -3000), y + 2500, name, size=8 if name in ("San Francisco", "Hetch Hetchy") else 7,
              weight="bold" if name in ("San Francisco", "Hetch Hetchy") else "normal", ha=ha, zorder=7)
k_m = 1000 / np.cos(np.radians(37.7)) if CRS == "EPSG:3857" else 1000
dmc.scalebar(ax, 25, loc=(0.04, 0.10), crs_units_per_km=k_m)
lg = [("San Francisco's power lines", dmc.GOLD, 1.8), ("Hetch Hetchy aqueduct", dmc.LAKE, 1.6), ("Other lines, 115 kV and up", dmc.STONE, 0.6)]
for i, (t, c, w) in enumerate(lg):
    y = 0.73 - i * 0.03
    fig.add_artist(__import__("matplotlib").lines.Line2D([0.72, 0.75], [y, y], transform=fig.transFigure, color=c, lw=w + 0.6))
    fig.text(0.758, y, t, size=8, va="center")

dmc.frame(
    fig, DAY,
    subtitle=("San Francisco dams the Tuolumne in Yosemite, sends the water west by gravity to its taps, and drops it\n"
              "through powerhouses below Groveland on the way. Its own lines carry the power to the Bay."),
    source="OpenStreetMap contributors (Overpass API) · " + (basemap.CREDIT if drawn else "Copernicus DEM GLO-90"),
    note="Lines and pipes as mapped in OpenStreetMap; operator tags decide what counts as San Francisco's.",
)
dmc.save(fig, DAY, alt=(
    ("Map" if drawn else "Shaded-relief map") + f" from San Francisco east to Yosemite. Gold lines are San Francisco's power lines running "
    f"from powerhouses below Groveland to the Bay Area; blue is the Hetch Hetchy aqueduct; grey is every other "
    f"high-voltage line in the region."))
