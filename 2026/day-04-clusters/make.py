"""
Day 4 · Clusters — Rainier's swarms

Every earthquake the USGS has located within 30 km of Mount Rainier's summit since 2000,
grouped into swarms: bursts of earthquakes close together in both place and time.
DBSCAN on (east km, north km, days), scaled so 2 km ≈ 4 days; a swarm needs 25+ quakes.

Downloads (cached in data/): USGS ComCat catalog, Copernicus 30 m DEM.
"""
import io
import sys
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import terrain  # noqa: E402

import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.patches import Circle  # noqa: E402
from pyproj import Transformer  # noqa: E402
from sklearn.cluster import DBSCAN  # noqa: E402

DAY = 4
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
SUMMIT = (-121.7603, 46.8529)                       # Columbia Crest
RADIUS_KM = 30
UA = {"User-Agent": "30DayMapChallenge-2026 (github.com/bdgroves/30DayMapChallenge)"}
TODAY = datetime.now(timezone.utc).date().isoformat()

# ── earthquakes ──────────────────────────────────────────────────────────────
cat = DATA / "comcat.csv"
if not cat.exists():
    url = ("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=2000-01-01"
           f"&endtime={TODAY}&latitude={SUMMIT[1]}&longitude={SUMMIT[0]}&maxradiuskm={RADIUS_KM}"
           "&orderby=time-asc&limit=20000&eventtype=earthquake")
    with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=180) as r:
        cat.write_bytes(r.read())
q = pd.read_csv(cat, parse_dates=["time"])
q["time"] = q["time"].dt.tz_localize(None) if q["time"].dt.tz is not None else q["time"]
to_utm = Transformer.from_crs(4326, 32610, always_xy=True)
q["x"], q["y"] = to_utm.transform(q["longitude"].values, q["latitude"].values)
sx, sy = to_utm.transform(*SUMMIT)
q["t"] = (q["time"] - q["time"].min()).dt.total_seconds() / 86400
print(f"  {len(q)} earthquakes, {q['time'].min():%Y-%m-%d} to {q['time'].max():%Y-%m-%d}")

# ── swarms ───────────────────────────────────────────────────────────────────
X = np.c_[(q["x"] - sx) / 2000, (q["y"] - sy) / 2000, q["t"] / 4]
q["c"] = DBSCAN(eps=1.0, min_samples=8).fit_predict(X)
sizes = q[q["c"] >= 0].groupby("c").size()
swarms = sizes[sizes >= 25].sort_values(ascending=False)
info = []
for c in swarms.index:
    g = q[q["c"] == c]
    info.append(dict(c=c, n=len(g), start=g["time"].min(), end=g["time"].max(), mmax=g["mag"].max(),
                     x=g["x"].median(), y=g["y"].median(), depth=g["depth"].median()))
info.sort(key=lambda d: -d["n"])
top = info[:5]
colour = {d["c"]: dmc.CATEGORICAL[i] for i, d in enumerate(top)}
for d in info:
    print(f"  swarm {d['start']:%b %Y}: {d['n']} quakes, M{d['mmax']:.1f} max, {d['depth']:.1f} km deep")
in_swarm = q["c"].isin(swarms.index)

# ── figure ───────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("portrait", map_box=(0.1625, 0.27, 0.675, 0.54))     # square: 5.4 × 5.4 in
r = (RADIUS_KM + 3) * 1000
z, tf = terrain.dem((SUMMIT[0] - 0.47, SUMMIT[1] - 0.32, SUMMIT[0] + 0.47, SUMMIT[1] + 0.32), res=30)
ax.imshow(terrain.relief(z, 30, strength=0.6), extent=terrain.extent(tf, z.shape), interpolation="bilinear")
ax.set_xlim(sx - r, sx + r)
ax.set_ylim(sy - r, sy + r)
ax.add_patch(Circle((sx, sy), RADIUS_KM * 1000, fill=False, ec=dmc.STONE, lw=0.6, ls=(0, (3, 3))))
dmc.label(ax, sx + RADIUS_KM * 1000 * 0.72, sy - RADIUS_KM * 1000 * 0.72, f"{RADIUS_KM} km", size=7, color=dmc.STONE)

bg = q[~in_swarm]
ax.scatter(bg["x"], bg["y"], s=2 + 1.2 * np.clip(bg["mag"], 0, 4) ** 2, color=dmc.ASH, alpha=0.35, lw=0)
for d in reversed(top):
    g = q[q["c"] == d["c"]]
    ax.scatter(g["x"], g["y"], s=3 + 1.6 * np.clip(g["mag"], 0, 4) ** 2, color=colour[d["c"]], alpha=0.75,
               lw=0, zorder=3)
other = q[in_swarm & ~q["c"].isin(colour)]
ax.scatter(other["x"], other["y"], s=3, color=dmc.INK, alpha=0.5, lw=0, zorder=2)

ax.scatter([sx], [sy], marker="^", s=90, color=dmc.INK, edgecolor=dmc.PARCHMENT, lw=0.8, zorder=5)
dmc.label(ax, sx + 1500, sy + 1500, "Mount Rainier", size=8.5, weight="bold")
for name, (lon, lat) in {"Paradise": (-121.735, 46.786), "Sunrise": (-121.524, 46.915),
                         "Longmire": (-121.811, 46.750), "Ashford": (-122.031, 46.759),
                         "Carbonado": (-122.050, 47.080), "Packwood": (-121.673, 46.608)}.items():
    x, y = to_utm.transform(lon, lat)
    ax.scatter([x], [y], s=6, color=dmc.INK, zorder=4)
    dmc.label(ax, x + 900, y, name, size=7, va="center")

# legend of swarms
lx = 0.055
fig.text(lx, 0.245, "THE BIGGEST CLUSTERS", family=dmc.MONO, size=7, color=dmc.STONE)
for i, d in enumerate(top):
    y = 0.226 - i * 0.017
    fig.patches.append(plt.Circle((lx + 0.006, y + 0.004), 0.004, color=colour[d["c"]], transform=fig.transFigure,
                                  figure=fig))
    when = f"{d['start']:%b %Y}" if d["start"].strftime("%Y%m") == d["end"].strftime("%Y%m") else \
        f"{d['start']:%b}–{d['end']:%b %Y}" if d["start"].year == d["end"].year else f"{d['start']:%b %Y}–{d['end']:%b %Y}"
    fig.text(lx + 0.018, y, f"{when}", family=dmc.TEXT, size=7.5, va="center")
    days = max(1, (d["end"] - d["start"]).days + 1)
    fig.text(lx + 0.20, y, f"{d['n']:,} quakes · largest M{d['mmax']:.1f} · {days} days",
             family=dmc.MONO, size=6.8, color=dmc.STONE, va="center")

# timeline: quakes per month, swarms coloured
tax = fig.add_axes((0.56, 0.115, 0.39, 0.13))
months = pd.period_range(q["time"].min().to_period("M"), q["time"].max().to_period("M"), freq="M")
q["m"] = q["time"].dt.to_period("M")
base = q[~q["c"].isin(colour)].groupby("m").size().reindex(months, fill_value=0)
xs = months.to_timestamp()
tax.bar(xs, base.values, width=28, color=dmc.ASH, alpha=0.6, lw=0)
bottom = base.values.astype(float)
for d in top:
    s = q[q["c"] == d["c"]].groupby("m").size().reindex(months, fill_value=0).values
    tax.bar(xs, s, width=28, bottom=bottom, color=colour[d["c"]], lw=0)
    bottom += s
tax.set_xlim(xs.min() - pd.Timedelta(days=60), xs.max() + pd.Timedelta(days=60))
tax.set_yscale("symlog", linthresh=10)
tax.set_yticks([0, 10, 100, 1000])
tax.set_yticklabels(["0", "10", "100", "1,000"])
for s in ("top", "right"):
    tax.spines[s].set_visible(False)
tax.spines["left"].set_color(dmc.MIST)
tax.spines["bottom"].set_color(dmc.MIST)
tax.tick_params(labelsize=6.5, colors=dmc.STONE, length=2)
for lab in tax.get_xticklabels() + tax.get_yticklabels():
    lab.set_fontfamily(dmc.MONO)
tax.set_title("EARTHQUAKES PER MONTH", loc="left", family=dmc.MONO, size=7, color=dmc.STONE, pad=4)

big = top[0]
yrs = q["time"].dt.year
dmc.frame(
    fig, DAY,
    subtitle=(f"{len(q):,} earthquakes located within {RADIUS_KM} km of the summit since 2000. Many come in clusters,\n"
              f"bursts close together in place and time; the largest, the swarm of {big['start']:%B %Y}, was {big['n']:,} quakes."),
    source="USGS ComCat (PNSN locations) · Copernicus DEM GLO-30",
    note="Clusters found with DBSCAN: quakes within about 2 km and 4 days of each other, 25 or more to count. Oct 2006 is an M4.5 and its aftershocks.",
)
dmc.save(fig, DAY, alt=(
    f"Shaded relief map of Mount Rainier with {len(q):,} earthquakes since 2000 as dots within a 30 km circle. "
    f"Most are grey background quakes, with coloured clusters marking swarms; the largest, {big['n']:,} quakes "
    f"in {big['start']:%B %Y}, sits {'just under' if np.hypot(big['x']-sx, big['y']-sy) < 5000 else 'near'} the summit. "
    f"A bar chart of quakes per month shows a spike for each swarm."))
