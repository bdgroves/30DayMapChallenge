"""
Day 5 · Sight — Where you can see Rainier from

Every place in the Pacific Northwest with a clear line of sight to the summit of Mount Rainier,
for someone standing on the ground (eyes 2 m up), allowing for the curve of the Earth and the
usual bending of light by the atmosphere (GDAL's default refraction coefficient, 0.857).
Terrain only: trees, buildings and weather are not in it.

Downloads (cached): Copernicus DEM GLO-90 tiles. Needs the GDAL command-line tools.
"""
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import terrain  # noqa: E402

import numpy as np  # noqa: E402
import rasterio  # noqa: E402
from matplotlib.colors import ListedColormap  # noqa: E402
from pyproj import Transformer  # noqa: E402
from rasterio.enums import Resampling  # noqa: E402

DAY = 5
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
CRS = "EPSG:32610"
SUMMIT = (-121.7603, 46.8529)
SUMMIT_M = 4392.0                                   # Columbia Crest, NAVD88
BOUNDS = (-125.6, 43.9, -116.6, 50.2)               # lon/lat area searched
MAX_KM = 350
SHOW = 4                                            # display at 4 × 90 m


def run(*cmd):
    print("  $", " ".join(str(c) for c in cmd[:3]), "…")
    subprocess.run([str(c) for c in cmd], check=True, capture_output=True)


to_utm = Transformer.from_crs(4326, CRS, always_xy=True)
sx, sy = to_utm.transform(*SUMMIT)
dem_utm = DATA / "dem_utm_90.tif"
view = DATA / "viewshed.tif"

if not dem_utm.exists():
    paths = terrain.tiles(BOUNDS, res=90)
    vrt = DATA / "mosaic.vrt"
    run("gdalbuildvrt", "-q", vrt, *paths)
    l, b = to_utm.transform(BOUNDS[0] + 1.2, BOUNDS[1] + 0.3)
    r, t = to_utm.transform(BOUNDS[2] - 1.2, BOUNDS[3] - 0.3)
    run("gdalwarp", "-q", "-t_srs", CRS, "-tr", 90, 90, "-r", "bilinear", "-te", l - 60000, b, r + 60000, t,
        "-wo", "INIT_DEST=0", "-co", "COMPRESS=DEFLATE", "-co", "TILED=YES", "-co", "BIGTIFF=IF_SAFER", vrt, dem_utm)

with rasterio.open(dem_utm) as src:
    row, col = src.index(sx, sy)
    win = src.read(1, window=((row - 3, row + 4), (col - 3, col + 4)))
    ground = float(win.max())
print(f"  DEM summit {ground:.0f} m; observer raised {SUMMIT_M - ground:.0f} m to {SUMMIT_M:.0f} m")

if not view.exists():
    run("gdal_viewshed", "-ox", sx, "-oy", sy, "-oz", max(1.0, SUMMIT_M - ground), "-tz", 2,
        "-md", MAX_KM * 1000, "-cc", 0.85714, "-vv", 255, "-iv", 0, "-ov", 0, "-co", "COMPRESS=DEFLATE",
        dem_utm, view)

# ── read at display resolution ───────────────────────────────────────────────
with rasterio.open(dem_utm) as src:
    h, w = src.height // SHOW, src.width // SHOW
    z = src.read(1, out_shape=(h, w), resampling=Resampling.average)
    tf = src.transform * src.transform.scale(src.width / w, src.height / h)
    full_tf, full_shape = src.transform, src.shape
with rasterio.open(view) as src:
    vis_full = src.read(1) == 255
    v = src.read(1, out_shape=(h, w), resampling=Resampling.max) == 255

cell_km2 = 0.09 * 0.09
area = vis_full.sum() * cell_km2
rows, cols = np.nonzero(vis_full)
xs = full_tf.c + (cols + 0.5) * full_tf.a
ys = full_tf.f + (rows + 0.5) * full_tf.e
dist = np.hypot(xs - sx, ys - sy)
far = int(np.argmax(dist))
to_ll = Transformer.from_crs(CRS, 4326, always_xy=True)
far_lon, far_lat = to_ll.transform(xs[far], ys[far])
print(f"  visible: {area:,.0f} km²; farthest {dist[far]/1000:.0f} km at {far_lat:.3f}, {far_lon:.3f}")


def seen(lon, lat, radius_cells=2):
    """Is the summit visible from (near) this point, on the full-resolution grid?"""
    x, y = to_utm.transform(lon, lat)
    c = int((x - full_tf.c) / full_tf.a)
    r = int((y - full_tf.f) / full_tf.e)
    if not (0 <= r < full_shape[0] and 0 <= c < full_shape[1]):
        return False
    return bool(vis_full[max(0, r - radius_cells):r + radius_cells + 1, max(0, c - radius_cells):c + radius_cells + 1].any())


places = {"Seattle": (-122.332, 47.606), "Tacoma": (-122.444, 47.253), "Lakewood": (-122.518, 47.172),
          "Olympia": (-122.900, 47.038), "Everett": (-122.202, 47.979), "Bellingham": (-122.488, 48.750),
          "Vancouver": (-123.121, 49.283), "Victoria": (-123.366, 48.428), "Portland": (-122.679, 45.515),
          "Salem": (-123.035, 44.943), "Yakima": (-120.506, 46.602), "Ellensburg": (-120.548, 46.997),
          "Wenatchee": (-120.311, 47.423), "Tri-Cities": (-119.137, 46.231), "Aberdeen": (-123.815, 46.975),
          "Port Angeles": (-123.430, 48.118), "Spokane": (-117.426, 47.658)}
vis_places = {k: seen(*v) for k, v in places.items()}
print("  " + ", ".join(f"{k} {'yes' if s else 'no'}" for k, s in vis_places.items()))

# ── figure ───────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("square", map_box=(0.03, 0.08, 0.94, 0.715))
ax.imshow(terrain.relief(z, 90 * SHOW, strength=0.55, exaggerate=2.2, water="#dde5e4"),
          extent=terrain.extent(tf, z.shape), interpolation="bilinear")
ax.imshow(np.where(v, 1.0, np.nan), extent=terrain.extent(tf, v.shape), cmap=ListedColormap([dmc.LAVA]),
          alpha=0.62, interpolation="nearest")
x0, y0 = to_utm.transform(-125.0, 44.9)
x1, y1 = to_utm.transform(-117.4, 49.45)
ax.set_xlim(x0, x1)
ax.set_ylim(y0, y1)
for km in (100, 200, 300):
    ax.add_patch(__import__("matplotlib").patches.Circle((sx, sy), km * 1000, fill=False, ec=dmc.STONE, lw=0.5,
                                                         ls=(0, (2, 3)), alpha=0.8))
    dmc.label(ax, sx + km * 1000 * 0.707, sy + km * 1000 * 0.707, f"{km} km", size=6.5, color=dmc.STONE)
ax.scatter([sx], [sy], marker="^", s=110, color=dmc.INK, edgecolor=dmc.PARCHMENT, lw=0.9, zorder=6)
dmc.label(ax, sx + 6000, sy - 9000, "Mount Rainier", size=8.5, weight="bold", zorder=6)
for name, (lon, lat) in places.items():
    x, y = to_utm.transform(lon, lat)
    ok = vis_places[name]
    ax.scatter([x], [y], s=14, color=dmc.LAVA if ok else dmc.PARCHMENT, edgecolor=dmc.INK, lw=0.6, zorder=6)
    left = name in ("Lakewood", "Olympia", "Aberdeen", "Victoria", "Salem", "Vancouver", "Port Angeles")
    dmc.label(ax, x + (-5000 if left else 5000), y + (-6000 if name == "Lakewood" else 0), name, size=7.5,
              ha="right" if left else "left", va="center", color=dmc.INK if ok else dmc.STONE, zorder=6)
fx, fy = xs[far], ys[far]
ax.plot([sx, fx], [sy, fy], color=dmc.INK, lw=0.6, ls=(0, (1, 2)), zorder=5)
ax.scatter([fx], [fy], s=10, color=dmc.INK, zorder=6)

seen_list = [k for k, s in vis_places.items() if s]
dmc.frame(
    fig, DAY,
    subtitle=(f"Every place within {MAX_KM} km with a clear line of sight to the summit: {area:,.0f} km² of ground, "
              f"in red.\nThe farthest is {dist[far]/1000:.0f} km away. Cities with a view in red, without in white."),
    source="Copernicus DEM GLO-90 · gdal_viewshed (Earth curvature, refraction 0.857)",
    note="Terrain only, at 90 m: trees, buildings and weather can hide the mountain from places marked as visible.",
)
dmc.save(fig, DAY, alt=(
    f"Shaded relief map of Washington and northern Oregon with the areas that can see Mount Rainier's summit "
    f"shaded red: {area:,.0f} square kilometres, reaching {dist[far]/1000:.0f} km at the farthest. "
    f"Cities with a line of sight: {', '.join(seen_list)}. "
    f"Cities without: {', '.join(k for k, s in vis_places.items() if not s)}."))
