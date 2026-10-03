"""
Day 28 · Feeling — Did you feel it? Nisqually, 2001

The M6.8 Nisqually earthquake of February 28, 2001, as the people who felt it reported it
to the USGS "Did You Feel It?" survey, averaged into 10 km squares.

Downloads (cached in data/): the USGS event and its DYFI product, Census state outlines,
Natural Earth for Canada. Writes out/map.png and out/alt.txt.
"""
import io
import json
import sys
import urllib.request
import zipfile
from datetime import datetime, timezone
from pathlib import Path
from zoneinfo import ZoneInfo

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import basemap  # noqa: E402
import dmc  # noqa: E402

import geopandas as gpd  # noqa: E402
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
from matplotlib.colors import BoundaryNorm, ListedColormap  # noqa: E402
from shapely.geometry import Point, box, shape  # noqa: E402

DAY = 28
EVENT = "uw10530748"
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
UA = {"User-Agent": "30DayMapChallenge-2026 (github.com/bdgroves/30DayMapChallenge)"}
MAPBOX = basemap.available()                      # Mapbox basemaps are Web Mercator
CRS = "EPSG:3857" if MAPBOX else "+proj=lcc +lat_1=45 +lat_2=49 +lat_0=47 +lon_0=-121.5 +datum=WGS84 +units=m"
EXTENT = (-125.2, 44.6, -116.4, 49.6)                     # lon/lat box shown


def fetch(url: str, name: str) -> bytes:
    p = DATA / name
    if not p.exists():
        print(f"  downloading {name}")
        with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=120) as r:
            p.write_bytes(r.read())
    return p.read_bytes()


def shapes(url: str, name: str) -> gpd.GeoDataFrame:
    z = zipfile.ZipFile(io.BytesIO(fetch(url, name)))
    shp = [n for n in z.namelist() if n.endswith(".shp")][0]
    folder = DATA / name.replace(".zip", "")
    if not folder.exists():
        z.extractall(folder)
    return gpd.read_file(folder / shp)


# ── the earthquake and its DYFI product ──────────────────────────────────────
ev = json.loads(fetch(f"https://earthquake.usgs.gov/fdsnws/event/1/query?eventid={EVENT}&format=geojson",
                      "event.json"))
props = ev["properties"]
lon, lat, depth = ev["geometry"]["coordinates"]
when = datetime.fromtimestamp(props["time"] / 1000, timezone.utc).astimezone(ZoneInfo("America/Los_Angeles"))
dyfi = props["products"]["dyfi"][0]
contents = dyfi["contents"]
geo = next(k for k in ("dyfi_geo_10km.geojson", "dyfi_geo.geojson", "dyfi_geo_1km.geojson") if k in contents)
def closed(rings):
    """USGS's DYFI squares sometimes leave a ring unclosed; close it."""
    return [r + [r[0]] if r and r[0] != r[-1] else r for r in rings]


raw = json.loads(fetch(contents[geo]["url"], geo))
recs = []
for f in raw["features"]:
    g = f["geometry"]
    if g["type"] == "Polygon":
        g = {"type": "Polygon", "coordinates": closed(g["coordinates"])}
    elif g["type"] == "MultiPolygon":
        g = {"type": "MultiPolygon", "coordinates": [closed(p) for p in g["coordinates"]]}
    recs.append({**f["properties"], "geometry": shape(g)})
cells = gpd.GeoDataFrame(recs, crs=4326)
responses = int(dyfi["properties"].get("numResp") or cells["nresp"].sum())
cells = cells[cells["nresp"] > 0].to_crs(CRS)
print(f"  {props['title']}: {len(cells)} cells, {responses} responses ({geo})")

# ── base map ─────────────────────────────────────────────────────────────────
states = shapes("https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_state_500k.zip", "states.zip")
states = states[states["STUSPS"].isin(["WA", "OR", "ID", "MT", "NV", "CA"])].to_crs(CRS)
world = shapes("https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_0_countries.zip", "countries.zip")
canada = world[world["ADMIN"] == "Canada"].to_crs(CRS)
frame_box = gpd.GeoSeries([box(*EXTENT)], crs=4326).to_crs(CRS).total_bounds

# intensity classes, II to VIII+, on the house heat ramp
bounds = [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 10.5]
names = ["II", "III", "IV", "V", "VI", "VII", "VIII+"]
words = ["weak", "weak", "light", "moderate", "strong", "very strong", "severe"]
colors = [dmc.SEQ_HEAT(x) for x in np.linspace(0.12, 1.0, len(names))]
cmap, norm = ListedColormap(colors), BoundaryNorm(bounds, len(names))

fig, ax = dmc.figure("square", map_box=(0.03, 0.09, 0.94, 0.715))
ax.set_xlim(frame_box[0], frame_box[2])
ax.set_ylim(frame_box[1], frame_box[3])
ax.set_aspect("equal")
if not (MAPBOX and basemap.mapbox(ax)):
    MAPBOX = False
    canada.plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5)
    states.plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5)
cells.plot(ax=ax, column="cdi", cmap=cmap, norm=norm, edgecolor="none", alpha=0.95)
if not MAPBOX:
    states.boundary.plot(ax=ax, color=dmc.WHITE, lw=0.6)
ax.set_xlim(frame_box[0], frame_box[2])
ax.set_ylim(frame_box[1], frame_box[3])

epi = gpd.GeoSeries([Point(lon, lat)], crs=4326).to_crs(CRS).iloc[0]
ax.scatter([epi.x], [epi.y], marker="*", s=260, color=dmc.INK, edgecolor=dmc.PARCHMENT, lw=1.2, zorder=5)
dmc.label(ax, epi.x + 9000, epi.y - 16000, f"Epicenter, {depth:.0f} km down", size=7.5, style="italic")

cities = {"Seattle": (-122.332, 47.606), "Tacoma": (-122.444, 47.253), "Olympia": (-122.900, 47.038),
          "Portland": (-122.679, 45.515), "Vancouver": (-123.121, 49.283), "Spokane": (-117.426, 47.658),
          "Yakima": (-120.506, 46.602), "Bellingham": (-122.488, 48.750), "Bend": (-121.315, 44.058),
          "Wenatchee": (-120.311, 47.423), "Lakewood": (-122.518, 47.172)}
for name, (x, y) in cities.items():
    p = gpd.GeoSeries([Point(x, y)], crs=4326).to_crs(CRS).iloc[0]
    if frame_box[0] < p.x < frame_box[2] and frame_box[1] < p.y < frame_box[3]:
        ax.scatter([p.x], [p.y], s=7, color=dmc.INK, zorder=4)
        dx, dy, ha = {"Lakewood": (-8000, 9000, "right"), "Olympia": (-7000, -3000, "right"),
                      "Vancouver": (-7000, 0, "right")}.get(name, (7000, 0, "left"))
        dmc.label(ax, p.x + dx, p.y + dy, name, size=7.5, ha=ha, va="center")

# legend: intensity scale
lax = fig.add_axes((0.69, 0.115, 0.26, 0.215))
lax.set_axis_off()
lax.add_patch(plt.Rectangle((-0.06, -0.04), 1.1, 1.1, color=dmc.PARCHMENT, alpha=0.93, transform=lax.transAxes,
                            clip_on=False))
lax.text(0, 1.0, "WHAT PEOPLE FELT", family=dmc.MONO, size=7, color=dmc.STONE, va="top")
for i, (n, w, c) in enumerate(zip(names, words, colors)):
    y = 0.84 - i * 0.12
    lax.add_patch(plt.Rectangle((0, y - 0.045), 0.1, 0.09, color=c, transform=lax.transAxes))
    lax.text(0.14, y, f"{n}", family=dmc.MONO, size=7.5, va="center", transform=lax.transAxes)
    lax.text(0.36, y, w, size=7.5, va="center", transform=lax.transAxes)

dmc.frame(
    fig, DAY,
    subtitle=(f"{when:%B} {when.day}, {when.year}, {when.hour % 12 or 12}:{when:%M} {'a.m' if when.hour < 12 else 'p.m'}. Magnitude {props['mag']:.1f}, "
              f"{depth:.0f} km beneath the south end of Puget Sound.\n"
              f"{responses:,} people told the USGS what they felt, averaged here into 10 km squares."),
    source=(f"USGS Did You Feel It? (event {EVENT}) · " +
            (basemap.CREDIT if MAPBOX else "U.S. Census Bureau · Natural Earth")),
)
dmc.save(fig, DAY, alt=(
    f"Map of the Pacific Northwest showing how strongly people felt the magnitude {props['mag']:.1f} "
    f"Nisqually earthquake of {when:%B} {when.day}, {when.year}. Small squares coloured from pale "
    f"(weak) to dark red (very strong) cover western Washington, strongest around the epicenter at the "
    f"south end of Puget Sound near Olympia, Lakewood and Tacoma, and fading toward Portland, "
    f"Vancouver and Spokane. Based on {responses:,} 'Did You Feel It?' reports."))
