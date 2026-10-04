"""
Day 6 · Vintage — Tasman to Cook

James Cook's "Chart of New-Zealand, explored in 1769 and 1770", the engraving published in 1772,
georeferenced from its own graticule and laid under today's coastline. Cook drew it on a Mercator
projection with longitude west of Greenwich, so its border is all the control it needs: the
meridians and parallels on the border fit a Mercator grid to within a few pixels.

Over it: today's coast in red, so you can see what Cook got right (almost everything) and what he
didn't (Banks Peninsula as an island, Stewart Island as a peninsula, and much of the coast drawn
about half a degree east of where it is, the longitude problem of the day). In gold, the stretches
of coast Abel Tasman saw in 1642-43, and an inset of his own coastline from the "Bonaparte map"
(c. 1644). A map version of my review of Paul Moon's "A Draught of the South Land" (Cartographic
Perspectives 105, 2025).

Downloads (cached in data/): the two charts from Wikimedia Commons (public domain: Royal Museums
Greenwich F0293; State Library of New South Wales), Natural Earth land.
"""
import io
import json
import sys
import urllib.parse
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
import numpy as np  # noqa: E402
from matplotlib.lines import Line2D  # noqa: E402
from PIL import Image  # noqa: E402
from shapely.geometry import LineString, MultiLineString, Point, Polygon, box  # noqa: E402

Image.MAX_IMAGE_PIXELS = None
DAY = 6
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
API = "https://commons.wikimedia.org/w/api.php"
COOK = "File:Chart of New Zealand, explored in 1769 and 1770 by Lieut. I- Cook,Commander of His Majesty's Bark Endeavour. RMG F0293.tiff"
TASMAN = "File:Tasman Map SLNSWSLNSW FL19288478.jpg"
WIDTH = 4000
R = 6378137.0                                       # Web Mercator sphere, metres


def commons(title, name):
    p = DATA / name
    if not p.exists():
        q = urllib.parse.urlencode(dict(action="query", prop="imageinfo", iiprop="url", iiurlwidth=WIDTH,
                                        titles=title, format="json"))
        js = fetch.json_get(f"{API}?{q}")
        page = next(iter(js["query"]["pages"].values()))
        fetch.get(page["imageinfo"][0]["thumburl"], p)
    return Image.open(p).convert("RGB")


cook = commons(COOK, "cook_1772.jpg")
tasman = commons(TASMAN, "tasman_1644.jpg")
s = cook.width / 1920                                # control points were read on a 1920-px copy
print(f"= Cook's chart {cook.size}, Tasman's {tasman.size}")

# ── georeferencing: Cook's graticule ────────────────────────────────────────
# Read off the border on a 1920-px copy: meridians 194°W..181°W, parallels 34°S..48°S (Mercator).
LON_W = np.array([194, 193, 192, 191, 190, 184, 183, 182, 181.])
LON_PX = np.array([283.3, 386.0, 485.3, 588.7, 689.0, 1295.7, 1395.7, 1499.3, 1598.7])
LAT_S = np.array([34, 35, 47, 48.])
LAT_PY = np.array([275.0, 399.0, 1954.0, 2100.7])
merc = lambda d: np.log(np.tan(np.pi / 4 + np.radians(d) / 2))       # noqa: E731
ax_, bx_ = np.polyfit(LON_W, LON_PX, 1)[::-1]
ay_, by_ = np.polyfit(merc(LAT_S), LAT_PY, 1)[::-1]
rx = LON_PX - (ax_ + bx_ * LON_W)
ry = LAT_PY - (ay_ + by_ * merc(LAT_S))
print(f"= graticule fit: x {np.abs(rx).max():.1f} px, y {np.abs(ry).max():.1f} px at most (1920-px copy)")


def px_to_m(x, y):
    """Pixel on the full image to Web Mercator metres (east longitude, south negative)."""
    lonw = (x / s - ax_) / bx_
    m = (y / s - ay_) / by_
    return R * np.radians(360 - lonw), -R * m


# the chart's frame, inside the border, on the 1920 copy
F = (186, 212, 1719, 2137)
fx0, fy1 = px_to_m(F[0] * s, F[1] * s)
fx1, fy0 = px_to_m(F[2] * s, F[3] * s)
chart = cook.crop((int(F[0] * s), int(F[1] * s), int(F[2] * s), int(F[3] * s)))

# ── today's coast, and Tasman's ─────────────────────────────────────────────
land = fetch.shapes(fetch.LAND, DATA / "land.zip")
nz = land.clip(box(165.5, -48.5, 179.5, -33.5)).explode(index_parts=False)
nz = nz[nz.area > 0.0005]
coast = nz.boundary.to_crs(3857)
west = Polygon([(165, -33.8), (172.7, -33.8), (173.6, -35.3), (174.3, -36.3), (174.75, -37.1), (175.0, -38.0),
                (174.9, -39.0), (174.4, -39.75), (165, -39.75)])
south_west = box(170.8, -42.25, 173.05, -40.4)        # Punakaiki north to Golden Bay
saw = nz.boundary.intersection(west).union_all().union(nz.boundary.intersection(south_west).union_all())
saw = gpd.GeoSeries([saw], crs=4326).to_crs(3857)
EVENTS = [  # Tasman's journal, 1642-43
    ("First sight of land, 13 Dec 1642", 171.35, -42.05, "left"),
    ("Murderers' Bay (Golden Bay), 18–19 Dec", 172.85, -40.78, "left"),
    ("Cape Maria van Diemen, 4 Jan 1643", 172.64, -34.48, "left"),
    ("Three Kings, 6 Jan", 172.13, -34.16, "left"),
]

# ── map ──────────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("portrait", map_box=(0.05, 0.085, 0.90, 0.745))
ax.imshow(chart, extent=(fx0, fx1, fy0, fy1), interpolation="lanczos", zorder=0)
coast.plot(ax=ax, color=dmc.LAVA, lw=0.75, alpha=0.9, zorder=3)
saw.plot(ax=ax, color=dmc.GOLD, lw=3.2, alpha=0.75, zorder=2)
for text, lo, la, side in EVENTS:
    p = gpd.GeoSeries([Point(lo, la)], crs=4326).to_crs(3857).iloc[0]
    ax.scatter([p.x], [p.y], s=16, color=dmc.GOLD, edgecolor=dmc.INK, lw=0.6, zorder=5)
    dmc.label(ax, p.x - 22000, p.y, text, size=6.6, color=dmc.INK, ha="right", va="center", style="italic",
              halo="#efe9dc", zorder=6)
ax.set_xlim(fx0, fx1)
ax.set_ylim(fy0, fy1)
ax.set_aspect("equal")

# Tasman's own coastline, from the Bonaparte map
t = tasman.width / 1920
tas = tasman.crop((int(1505 * t), int(1088 * t), int(1665 * t), int(1330 * t)))
iax = fig.add_axes((0.585, 0.105, 0.33, 0.26))
iax.imshow(tas)
iax.set_xticks([])
iax.set_yticks([])
for sp in iax.spines.values():
    sp.set_color(dmc.GOLD)
    sp.set_linewidth(1.2)
iax.set_title("TASMAN'S COAST, C. 1644", family=dmc.MONO, size=6.5, color=dmc.INK, loc="left", pad=3)

# legend
lx, ly = 0.07, 0.205
fig.lines.append(Line2D([lx, lx + 0.035], [ly, ly], color=dmc.LAVA, lw=1.2, transform=fig.transFigure))
fig.text(lx + 0.045, ly, "Today's coastline", size=7.5, va="center")
fig.lines.append(Line2D([lx, lx + 0.035], [ly - 0.022] * 2, color=dmc.GOLD, lw=3, alpha=0.75,
                        transform=fig.transFigure))
fig.text(lx + 0.045, ly - 0.022, "Coast Tasman saw, 1642–43", size=7.5, va="center")

dmc.frame(
    fig, DAY,
    subtitle=("James Cook's chart of the Endeavour's six months round New Zealand, engraved in 1772 and\n"
              "laid under today's coast in red. In gold, the stretches Abel Tasman saw 127 years before."),
    source="Cook chart: Royal Museums Greenwich F0293 · Tasman map: State Library of NSW · via Wikimedia Commons · Natural Earth",
    note="Georeferenced from Cook's own graticule. Much of his coast sits about half a degree east of where it is.",
)
dmc.save(fig, DAY, alt=(
    "James Cook's 1772 engraved chart of New Zealand, georeferenced, with today's coastline traced over it in red; "
    "the two match closely, except that Banks Peninsula is drawn as an island, Stewart Island as part of the "
    "mainland, and much of the coast sits slightly east. Gold lines mark the west coasts Abel Tasman saw in 1642-43, "
    "with an inset of Tasman's own coastline from the Bonaparte map of about 1644."))
