"""
Day 29 · Raster — When Rainier's snow melted

The spring and summer of 2026 on Mount Rainier, pixel by pixel: the date each 30 m patch of ground
lost its snow for the season, from every Sentinel-2 pass between March 1 and September 15 that
saw the ground. White is ground that never melted out, the glaciers and permanent snow.

Each pass comes with ESA's scene classification (SCL), which labels every 20 m pixel as snow,
clear ground, cloud and so on. For each pixel, the melt-out date is the first clear, snow-free
view after the last view with snow. Cloud, cloud shadow and terrain shadow count as no view.

Below the map, the ground truth from my Rainier snowpack tracker (STORM CHASER): Paradise SNOTEL's
snow water equivalent through water year 2026, against its 1991-2020 median, with the day the
satellite says Paradise's pixel melted out.

Downloads (cached in data/): Sentinel-2 L2A SCL from Element 84's Earth Search (AWS open data,
no login), the rainier-snowpack SNOTEL archive, Copernicus 30 m DEM.
"""
import json
import os
import sys
from datetime import date
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402
import terrain  # noqa: E402

import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
import rasterio  # noqa: E402
from matplotlib.colors import LinearSegmentedColormap  # noqa: E402
from pyproj import Transformer  # noqa: E402
from rasterio.transform import from_origin  # noqa: E402
from rasterio.vrt import WarpedVRT  # noqa: E402
from rasterio.warp import Resampling  # noqa: E402

DAY = 29
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
STAC = "https://earth-search.aws.element84.com/v1/search"
BBOX = (-121.99, 46.71, -121.52, 47.00)               # the mountain and its ring of valleys
START, END = "2026-03-01", "2026-09-15"
RES = 30
CRS = "EPSG:32610"
SUMMIT = (-121.7603, 46.8529)
PARADISE = (-121.74767, 46.78266)                       # SNOTEL 679, from the tracker's station list
SNOTEL = "https://raw.githubusercontent.com/bdgroves/rainier-snowpack/main/data/archive/snotel_wy2026.csv"
os.environ.update(GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR", AWS_NO_SIGN_REQUEST="YES", GDAL_HTTP_MAX_RETRY="4",
                  GDAL_HTTP_RETRY_DELAY="3", CPL_VSIL_CURL_ALLOWED_EXTENSIONS=".tif")

to = Transformer.from_crs(4326, CRS, always_xy=True)
l, b, r, t = to.transform_bounds(*BBOX)
l, t = np.floor(l / RES) * RES, np.ceil(t / RES) * RES
W, H = int((r - l) // RES), int((t - b) // RES)
TF = from_origin(l, t, RES, RES)

# ── the scenes ───────────────────────────────────────────────────────────────
stack_p = DATA / "scl_stack.npz"
if not stack_p.exists():
    items, body = [], {"collections": ["sentinel-2-l2a"], "bbox": list(BBOX), "datetime": f"{START}T00:00:00Z/{END}T23:59:59Z",
                       "query": {"eo:cloud_cover": {"lt": 80}}, "limit": 100}
    import urllib.request
    url = STAC
    while url:
        data = json.dumps(body).encode() if body is not None else None
        req = urllib.request.Request(url, data=data, headers={"Content-Type": "application/json"})
        with urllib.request.urlopen(req, timeout=120) as resp:
            js = json.loads(resp.read())
        items += js.get("features", [])
        nxt = [lk for lk in js.get("links", []) if lk.get("rel") == "next"]
        if not nxt:
            break
        url = nxt[0]["href"]
        body = nxt[0].get("body") if nxt[0].get("method", "GET").upper() == "POST" else None
    by_date = {}
    for it in items:
        by_date.setdefault(it["properties"]["datetime"][:10], []).append(it)
    print(f"  {len(items)} scenes on {len(by_date)} days", flush=True)
    dates, layers = [], []
    for d in sorted(by_date):
        code = np.zeros((H, W), np.uint8)             # 0 no view, 1 clear ground, 2 snow
        for it in by_date[d]:
            href = it["assets"]["scl"]["href"]
            try:
                with rasterio.open(href) as src, WarpedVRT(src, crs=CRS, transform=TF, width=W, height=H,
                                                           resampling=Resampling.nearest, nodata=0) as vrt:
                    scl = vrt.read(1)
            except Exception as e:  # noqa: BLE001
                print(f"  {d} {it['id']}: {e}")
                continue
            snow = scl == 11
            clear = np.isin(scl, (4, 5, 6))
            code = np.where(snow, 2, np.where(clear & (code == 0), 1, code)).astype(np.uint8)
        if (code > 0).mean() > 0.02:
            dates.append(d)
            layers.append(code)
    np.savez_compressed(stack_p, dates=np.array(dates), stack=np.stack(layers))
z_ = np.load(stack_p)
dates = [date.fromisoformat(d) for d in z_["dates"]]
stack = z_["stack"]
print(f"= {len(dates)} usable days {dates[0]}..{dates[-1]}; grid {W}x{H} at {RES} m")

# ── melt-out ─────────────────────────────────────────────────────────────────
n = len(dates)
# a pixel counts as snowy only when two views in a row (ignoring cloudy days) show snow, so one
# misclassified scene or a light dusting can't push its melt-out later
last_snow = np.full((H, W), -1, np.int16)                           # last confirmed view with snow
prev = np.zeros((H, W), np.uint8)                                   # previous clear-or-snow view
prev_i = np.full((H, W), -1, np.int16)
for i in range(n):
    c = stack[i]
    pair = (c == 2) & (prev == 2)
    last_snow[pair] = i
    seen = c > 0
    prev[seen] = c[seen]
    prev_i[seen] = i
first_clear_after = np.full((H, W), n, np.int16)                    # first clear view after it
for i in range(n):
    hit = (stack[i] == 1) & (i > last_snow) & (first_clear_after == n)
    first_clear_after[hit] = i
doy = np.array([d.toordinal() for d in dates])
melt = np.full((H, W), np.nan)
ok = (last_snow >= 0) & (first_clear_after < n)
melt[ok] = doy[first_clear_after[ok]]
persistent = (last_snow >= 0) & (first_clear_after == n) & (np.array(dates)[np.clip(last_snow, 0, n - 1)] >= date(2026, 8, 20))
views = (stack > 0).sum(axis=0)
print(f"= melt-out mapped for {ok.mean():.0%} of pixels; never melted (glaciers, permanent snow) {persistent.mean():.1%}; "
      f"median clear views per pixel {np.median(views):.0f}")

# Paradise: satellite against the SNOTEL record
px, py = to.transform(*PARADISE)
pc, pr = int((px - l) // RES), int((t - py) // RES)
win = melt[pr - 1:pr + 2, pc - 1:pc + 2]
sat_melt = date.fromordinal(int(np.nanmedian(win))) if np.isfinite(win).any() else None
fetch.get(SNOTEL, DATA / "snotel_wy2026.csv")
sn = pd.read_csv(DATA / "snotel_wy2026.csv", parse_dates=["date"])
par = sn[sn["station_name"] == "Paradise"].sort_values("date")
peak_i = par["swe_in"].idxmax()
after_peak = par.loc[peak_i:]
gone = after_peak[after_peak["swe_in"] <= 0]
snotel_melt = gone["date"].iloc[0].date() if len(gone) else None
print(f"= Paradise: SNOTEL peak {par.loc[peak_i, 'swe_in']:.1f} in on {par.loc[peak_i, 'date'].date()}, "
      f"snow gone {snotel_melt}; satellite melt-out at the station's pixel {sat_melt}")

# ── map ──────────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("portrait", map_box=(0.05, 0.285, 0.90, 0.525))
zz, ztf = terrain.dem((BBOX[0] - 0.03, BBOX[1] - 0.03, BBOX[2] + 0.03, BBOX[3] + 0.03), crs=CRS, res=RES)
ax.imshow(terrain.relief(zz, RES, strength=0.7, exaggerate=1.2), extent=terrain.extent(ztf, zz.shape), interpolation="bilinear", zorder=0)
cmap = LinearSegmentedColormap.from_list("melt", ["#e8c48a", "#c9a46a", "#7fb0c4", dmc.LAKE, "#1f3a4a"])
lo, hi = date(2026, 4, 1).toordinal(), date(2026, 8, 31).toordinal()
m = np.ma.masked_invalid(melt)
ax.imshow(m, extent=(l, l + W * RES, t - H * RES, t), cmap=cmap, vmin=lo, vmax=hi, alpha=0.78, interpolation="nearest", zorder=1)
ax.imshow(np.ma.masked_where(~persistent, np.ones_like(melt)), extent=(l, l + W * RES, t - H * RES, t),
          cmap=LinearSegmentedColormap.from_list("w", ["#ffffff", "#ffffff"]), alpha=0.95, interpolation="nearest", zorder=2)
for name, lon, lat in [("Paradise", *PARADISE), ("Columbia Crest", *SUMMIT), ("Sunrise", -121.6408, 46.9147),
                       ("Mowich Lake", -121.8636, 46.9327), ("Longmire", -121.8113, 46.7505)]:
    x, y = to.transform(lon, lat)
    ax.scatter([x], [y], s=12, color=dmc.INK, zorder=4)
    dmc.label(ax, x + 450, y + 250, name, size=7.5, zorder=5)
ax.set_xlim(l, l + W * RES)
ax.set_ylim(t - H * RES, t)
ax.set_aspect("equal")
dmc.scalebar(ax, 5, loc=(0.05, 0.05))

# colour key, by month
kax = fig.add_axes((0.30, 0.235, 0.40, 0.010))
kax.imshow(np.linspace(0, 1, 256)[None], aspect="auto", cmap=cmap, extent=(lo, hi, 0, 1))
kax.set_yticks([])
ticks = [date(2026, mth, 1).toordinal() for mth in range(4, 10)]
kax.set_xticks(ticks[:-1])
kax.set_xticklabels(["Apr", "May", "Jun", "Jul", "Aug"], family=dmc.MONO, fontsize=7, color=dmc.STONE)
for s in kax.spines.values():
    s.set_visible(False)
fig.text(0.30, 0.252, "SNOW GONE BY", family=dmc.MONO, size=7, color=dmc.STONE)
fig.add_artist(__import__("matplotlib").patches.Rectangle((0.728, 0.234), 0.018, 0.012, facecolor="#ffffff", edgecolor=dmc.STONE,
                                                           lw=0.6, transform=fig.transFigure))
fig.text(0.755, 0.240, "Never melted", size=7.5, va="center", color=dmc.INK)

# Paradise SNOTEL strip
sx = fig.add_axes((0.10, 0.105, 0.80, 0.085))
sx.fill_between(par["date"], par["swe_in"], color=dmc.LAKE, alpha=0.25, lw=0)
sx.plot(par["date"], par["swe_in"], color=dmc.LAKE, lw=1.2, label="Water year 2026")
sx.plot(par["date"], par["median_swe_in"], color=dmc.STONE, lw=0.9, ls=(0, (3, 2)), label="1991-2020 median")
if sat_melt:
    sx.axvline(pd.Timestamp(sat_melt), color=dmc.LAVA, lw=1)
    sx.text(pd.Timestamp(sat_melt), sx.get_ylim()[1] * 0.95,
            f"  Paradise melts out\n  satellite {sat_melt:%b} {sat_melt.day}" + (f", SNOTEL {snotel_melt:%b} {snotel_melt.day}"
                                                                                 if snotel_melt else ""),
            family=dmc.MONO, size=6.3, color=dmc.LAVA, va="top", linespacing=1.4)
for s in ("top", "right"):
    sx.spines[s].set_visible(False)
sx.tick_params(labelsize=6.5, colors=dmc.STONE, length=2)
for tl in sx.get_xticklabels() + sx.get_yticklabels():
    tl.set_fontfamily(dmc.MONO)
import matplotlib.dates as mdates  # noqa: E402
sx.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
sx.legend(loc="upper left", fontsize=6.5, frameon=False)
fig.text(0.10, 0.200, "PARADISE SNOTEL, SNOW WATER EQUIVALENT (INCHES), FROM MY RAINIER SNOWPACK TRACKER", family=dmc.MONO,
         size=7, color=dmc.STONE)

dmc.frame(
    fig, DAY,
    title="When Rainier's snow melted",
    subtitle=(f"Every 30 m patch of the mountain coloured by the day it lost its snow in 2026, from {len(dates)} days\n"
              f"of Sentinel-2 views. The glaciers, in white, never did."),
    source="Copernicus Sentinel-2 L2A and DEM · NRCS SNOTEL via my snowpack tracker",
    note="Uncoloured: no snow seen after March 1, or snow hidden under forest. Snow counts when two clear views in a row show it.",
)
dmc.save(fig, DAY, alt=(
    "Map of Mount Rainier with every 30 metre patch coloured by the date its snow melted in 2026, from tan in April in the "
    "valleys to deep blue in August high on the mountain, with the glaciers in white where the snow never melted. Below, "
    "Paradise SNOTEL's snow water equivalent through the 2026 water year against its median, with the satellite's "
    "melt-out date for Paradise marked."))
