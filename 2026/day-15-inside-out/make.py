"""
Day 15 · Inside out — Inside Kīlauea

Every earthquake the USGS has located within 20 km of Kīlauea's summit since 2019, seen from
above and then from the side: a north-south slice through Halemaʻumaʻu, 4 km thick, showing
how deep they are. The volcano's plumbing doesn't show at the surface; its earthquakes outline it.

Downloads (cached in data/): USGS ComCat, a year at a time; Copernicus 30 m DEM.
"""
import io
import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402
import terrain  # noqa: E402

import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.colors import LogNorm, Normalize  # noqa: E402
from pyproj import Transformer  # noqa: E402

DAY = 15
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
CRS = "EPSG:32605"
SUMMIT = (-155.2834, 19.4069)                  # Halemaʻumaʻu
RADIUS = 20
HALF = 2.0                                     # km either side of the slice
FIRST = 2019
NOW = datetime.now(timezone.utc)

frames = []
for y in range(FIRST, NOW.year + 1):
    end = f"{y + 1}-01-01" if y < NOW.year else NOW.strftime("%Y-%m-%dT%H:%M:%S")
    raw = fetch.get("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&eventtype=earthquake"
                    f"&starttime={y}-01-01&endtime={end}&latitude={SUMMIT[1]}&longitude={SUMMIT[0]}"
                    f"&maxradiuskm={RADIUS}&orderby=time-asc&limit=20000", DATA / f"comcat_{y}.csv")
    if raw.strip():
        f = pd.read_csv(io.BytesIO(raw))
        if len(f) >= 20000:
            print(f"! {y} hit the 20,000 limit; split it")
        frames.append(f)
q = pd.concat(frames, ignore_index=True)
q = q[q["depth"].between(-2, 15)]
to = Transformer.from_crs(4326, CRS, always_xy=True)
sx, sy = to.transform(*SUMMIT)
q["x"], q["y"] = to.transform(q["longitude"].values, q["latitude"].values)
q["e"], q["n"] = (q["x"] - sx) / 1000, (q["y"] - sy) / 1000
sl = q[q["e"].abs() <= HALF]
print(f"= {len(q):,} earthquakes {FIRST}-{NOW.year}, M{q['mag'].min():.1f}-{q['mag'].max():.1f}; "
      f"{len(sl):,} in the slice; depth median {q['depth'].median():.1f} km")

# ── figure ───────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("wide", map_box=(0.03, 0.08, 0.40, 0.72))
R = RADIUS * 1000
bb = (SUMMIT[0] - 0.22, SUMMIT[1] - 0.2, SUMMIT[0] + 0.22, SUMMIT[1] + 0.2)
z, tf = terrain.dem(bb, crs=CRS, res=60, src_res=30)
ax.imshow(terrain.relief(z, 60, strength=0.6, exaggerate=2.0), extent=terrain.extent(tf, z.shape),
          interpolation="bilinear", zorder=0)
dnorm = Normalize(0, 10)
o = q.sort_values("depth", ascending=False)
ax.scatter(o["x"], o["y"], s=1.2, c=o["depth"], cmap=dmc.SEQ_HEAT.reversed(), norm=dnorm, lw=0, alpha=0.6, zorder=2)
ax.add_patch(__import__("matplotlib").patches.Rectangle((sx - HALF * 1000, sy - R), 2 * HALF * 1000, 2 * R, fill=False,
             ec=dmc.INK, lw=0.8, ls="dashed", zorder=4))
dmc.label(ax, sx, sy + R - 1200, "N", size=9, weight="bold", ha="center", zorder=5)
dmc.label(ax, sx, sy - R + 1200, "S", size=9, weight="bold", ha="center", zorder=5)
dmc.label(ax, sx + 2600, sy + 600, "Halemaʻumaʻu", size=7.5, style="italic", zorder=5)
ax.set_xlim(sx - R, sx + R)
ax.set_ylim(sy - R, sy + R)
ax.set_aspect("equal")
dmc.scalebar(ax, 5, loc=(0.05, 0.05))

cx = fig.add_axes((0.50, 0.14, 0.46, 0.60))
H, xe, ye = np.histogram2d(sl["n"], sl["depth"], bins=[np.arange(-RADIUS, RADIUS + 0.1, 0.25), np.arange(-1, 12.01, 0.125)])
cx.imshow(H.T, origin="upper", extent=(-RADIUS, RADIUS, 12, -1), aspect="auto", cmap=dmc.SEQ_HEAT,
          norm=LogNorm(1, max(2, H.max())), interpolation="nearest")
cx.set_ylim(12, -1)
cx.set_xlim(-RADIUS, RADIUS)
cx.axhline(0, color=dmc.STONE, lw=0.6)
cx.set_xlabel("km south ←    → km north of Halemaʻumaʻu", family=dmc.MONO, fontsize=7.5, color=dmc.STONE)
cx.set_ylabel("depth below sea level, km", family=dmc.MONO, fontsize=7.5, color=dmc.STONE)
cx.tick_params(labelsize=7, colors=dmc.STONE)
for t in cx.get_xticklabels() + cx.get_yticklabels():
    t.set_fontfamily(dmc.MONO)
for s in ("top", "right"):
    cx.spines[s].set_visible(False)
fig.text(0.50, 0.765, f"THE SLICE, SEEN FROM THE EAST  ·  {len(sl):,} EARTHQUAKES WITHIN {HALF:g} KM OF THE LINE",
         family=dmc.MONO, size=6.8, color=dmc.STONE)

dmc.frame(
    fig, DAY,
    subtitle=(f"{len(q):,} earthquakes beneath Kīlauea since {FIRST}, from above (left, darker is shallower) and side-on\n"
              f"(right, darker is more earthquakes). Where magma is stored and moves, the rock cracks."),
    source="USGS ComCat (located by the Hawaiian Volcano Observatory) · Copernicus DEM GLO-30",
    note="Depths are below sea level; the summit rim stands about 1.2 km above it.",
)
dmc.save(fig, DAY, alt=(
    f"Left, a shaded-relief map of Kīlauea's summit dotted with {len(q):,} earthquake epicentres since {FIRST}; "
    f"right, a north-south cross-section through Halemaʻumaʻu showing where the earthquakes cluster with depth."))
