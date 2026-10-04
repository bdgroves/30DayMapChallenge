"""
Day 17 · Light & dark — Great Basin's dark sky

Nevada and Utah at night from NASA's Black Marble, with Great Basin National Park, an
International Dark Sky Park since 2016, in the dark between the glow of Las Vegas and Salt
Lake City. Under the map, one line from Las Vegas through Wheeler Peak to Salt Lake City, and
how bright the night is along it.

The image is NASA's Black Marble composite as NASA GIBS serves it (no login needed). It is a
picture of the night, not a measurement: the profile reads the brightness of the image, 0 to
255, not radiance. With an Earthdata login (EARTHDATA_* secrets, as in rainier-snowpack) this
could move to the VNP46A4 annual radiance product.

Downloads (cached in data/): NASA GIBS WMS tiles, Census state outlines.
"""
import io
import re
import sys
import urllib.parse
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import numpy as np  # noqa: E402
from PIL import Image  # noqa: E402

DAY = 17
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
WMS = "https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi"
W, S, E, N = -120.2, 34.9, -108.9, 42.15             # Nevada and Utah
RES = 0.004                                           # degrees per pixel, about 400 m
WHEELER = (-114.3137, 38.9858)
LV, SLC = (-115.1398, 36.1699), (-111.8910, 40.7608)

# which Black Marble layer GIBS has
cap = fetch.get(WMS + "?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.3.0", DATA / "gibs_caps.xml").decode("utf-8", "replace")
names = sorted(set(re.findall(r"<Name>([^<]*Black_Marble[^<]*)</Name>", cap)))
LAYER = "VIIRS_Black_Marble" if "VIIRS_Black_Marble" in names else (names[0] if names else "VIIRS_Black_Marble")
blk = cap[cap.find(f"<Name>{LAYER}</Name>"):][:4000]
times = re.findall(r'<Dimension name="time"[^>]*>([^<]*)<', blk)
print(f"= GIBS Black Marble layers {names}; using {LAYER}, times {times[:1]}")

# the image, in four tiles
nx, ny = int(round((E - W) / RES)), int(round((N - S) / RES))
img = np.zeros((ny, nx, 3), dtype=np.uint8)
for i, (x0, x1) in enumerate([(0, nx // 2), (nx // 2, nx)]):
    for j, (y0, y1) in enumerate([(0, ny // 2), (ny // 2, ny)]):
        bw, be = W + x0 * RES, W + x1 * RES
        bn, bs = N - y0 * RES, N - y1 * RES
        q = urllib.parse.urlencode({"SERVICE": "WMS", "REQUEST": "GetMap", "VERSION": "1.3.0", "LAYERS": LAYER, "STYLES": "",
                                    "CRS": "EPSG:4326", "BBOX": f"{bs},{bw},{bn},{be}", "WIDTH": x1 - x0, "HEIGHT": y1 - y0,
                                    "FORMAT": "image/png"})
        b = fetch.get(f"{WMS}?{q}", DATA / f"bm_{i}{j}.png")
        img[y0:y1, x0:x1] = np.asarray(Image.open(io.BytesIO(b)).convert("RGB"))
lum = img.astype(float) @ [0.299, 0.587, 0.114]
print(f"= image {nx}x{ny}, brightness median {np.median(lum):.0f}, 99th pct {np.percentile(lum, 99):.0f}")


def sample(a, b, n=600):
    lon = np.linspace(a[0], b[0], n)
    lat = np.linspace(a[1], b[1], n)
    c = np.clip(((lon - W) / RES).astype(int), 0, nx - 1)
    r = np.clip(((N - lat) / RES).astype(int), 0, ny - 1)
    return lon, lat, lum[r, c]


def km(a, b):
    R, k = 6371.0, np.pi / 180
    return R * np.arccos(np.sin(a[1] * k) * np.sin(b[1] * k) + np.cos(a[1] * k) * np.cos(b[1] * k) * np.cos((b[0] - a[0]) * k))


lo1, la1, p1 = sample(LV, WHEELER)
lo2, la2, p2 = sample(WHEELER, SLC)
d1, d2 = km(LV, WHEELER), km(WHEELER, SLC)
dist = np.concatenate([np.linspace(0, d1, len(p1)), d1 + np.linspace(0, d2, len(p2))])
prof = np.concatenate([p1, p2])
dark = prof[(dist > 60) & (dist < d1 + d2 - 60)]
print(f"= LV→Wheeler {d1:.0f} km, Wheeler→SLC {d2:.0f} km; brightness at Wheeler {prof[len(p1)]:.0f}, "
      f"darkest stretch median {np.median(dark):.0f}")

# ── figure ───────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("portrait", dark=True, map_box=(0.05, 0.255, 0.90, 0.52))
ax.imshow(img, extent=(W, E, S, N), interpolation="lanczos", zorder=0)
st = fetch.shapes(fetch.STATES, DATA / "states.zip").to_crs(4326)
st.boundary.plot(ax=ax, color="#6b6560", lw=0.4, alpha=0.6, zorder=1)
ax.set_xlim(W, E)
ax.set_ylim(S, N)
ax.set_aspect(1 / np.cos(np.radians((S + N) / 2)))
ax.plot([LV[0], WHEELER[0], SLC[0]], [LV[1], WHEELER[1], SLC[1]], color=dmc.GOLD, lw=0.7, ls=(0, (4, 3)), alpha=0.8, zorder=2)
ax.scatter([WHEELER[0]], [WHEELER[1]], s=260, facecolor="none", edgecolor=dmc.GOLD, lw=1.2, zorder=3)
ax.text(WHEELER[0] + 0.32, WHEELER[1] + 0.05, "Great Basin\nNational Park", family=dmc.TEXT, size=9, color=dmc.GOLD,
        va="center", style="italic", zorder=4)
for name, lon, lat, ha, dx in [("Las Vegas", *LV, "left", 0.25), ("Salt Lake City", *SLC, "left", 0.25),
                                ("Reno", -119.81, 39.53, "left", 0.2), ("St. George", -113.58, 37.10, "left", 0.2),
                                ("Elko", -115.76, 40.83, "left", 0.15), ("Ely", -114.89, 39.25, "right", -0.15),
                                ("Tonopah", -117.23, 38.07, "left", 0.15), ("Provo", -111.66, 40.23, "left", 0.2)]:
    ax.text(lon + dx, lat, name, family=dmc.MONO, size=7, color=dmc.MIST, ha=ha, va="center", zorder=4)
for name, lon, lat in [("NEVADA", -117.0, 39.6), ("UTAH", -111.3, 38.9)]:
    ax.text(lon, lat, name, family=dmc.MONO, size=9, color="#4a4f57", ha="center", zorder=1)

# the profile
px = fig.add_axes((0.10, 0.115, 0.80, 0.095))
px.fill_between(dist, prof, color=dmc.GOLD, alpha=0.35, lw=0)
px.plot(dist, prof, color=dmc.GOLD, lw=0.8)
px.axvline(d1, color=dmc.MIST, lw=0.6, ls=(0, (2, 2)))
px.set_xlim(0, d1 + d2)
px.set_ylim(0, max(255, prof.max()))
px.set_facecolor(dmc.NIGHT)
for s in ("top", "right"):
    px.spines[s].set_visible(False)
for s in ("left", "bottom"):
    px.spines[s].set_color("#3a3e45")
px.tick_params(labelsize=6.5, colors=dmc.MIST, length=2)
for t in px.get_xticklabels() + px.get_yticklabels():
    t.set_fontfamily(dmc.MONO)
fig.text(0.10, 0.222, "HOW BRIGHT THE NIGHT IS ALONG THE DASHED LINE  (IMAGE BRIGHTNESS 0-255, BY KM)", family=dmc.MONO, size=7,
         color=dmc.MIST)
for x, t, ha in [(0, "Las Vegas", "left"), (d1, "Wheeler Peak", "center"), (d1 + d2, "Salt Lake City", "right")]:
    px.text(x, max(255, prof.max()) * 0.98, t, family=dmc.MONO, size=6.5, color=dmc.WHITE, ha=ha, va="top")

dmc.frame(
    fig, DAY,
    subtitle=(f"Nevada and Utah from space at night. Great Basin National Park, a Dark Sky Park since\n"
              f"2016, sits in the dark between Las Vegas, {d1:.0f} km away, and Salt Lake City, {d2:.0f} km."),
    source="NASA Black Marble 2016, via NASA GIBS · U.S. Census Bureau · DarkSky International",
    note="The profile reads the image's brightness, not measured radiance.",
)
dmc.save(fig, DAY, alt=(
    f"Night satellite image of Nevada and Utah: bright patches at Las Vegas, Reno, and the Salt Lake City to Provo corridor, "
    f"faint specks for small towns, and a dark expanse around Great Basin National Park, circled in gold. Below, a profile "
    f"of brightness along a line from Las Vegas through Wheeler Peak to Salt Lake City, high at both ends and near zero in the middle."))
