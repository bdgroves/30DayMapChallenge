"""Pleistocene Lake Lahontan at its highstand (1,335 m, about 15,000 years ago; USGS FS 2004-3044),
drawn by flooding today's Copernicus DEM to 1,335 m from Pyramid Lake. Writes out/lahontan.png."""
import sys
from pathlib import Path
import numpy as np
from scipy import ndimage
from matplotlib.colors import to_rgb
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from mapkit import *   # noqa: F403

HERE = Path(__file__).resolve().parent
OUT = HERE / "out"; OUT.mkdir(exist_ok=True)
HIGH = 1335

def highstand():
    z, ext = read_dem("lahontan")
    w, e, s, n = ext
    wet = np.nan_to_num(z, nan=9999) <= HIGH
    lab, _ = ndimage.label(wet)
    r = int((n - 40.0) / (n - s) * z.shape[0]); c = int((-119.55 - w) / (e - w) * z.shape[1])
    mask = lab == lab[r, c]
    cell = ((e - w) / z.shape[1] * 111.32 * np.cos(np.radians((s + n) / 2 + 0 * 0))) * ((n - s) / z.shape[0] * 110.54)
    lat = np.linspace(n, s, z.shape[0])[:, None]
    area = (mask * ((e - w) / z.shape[1] * 111.32 * np.cos(np.radians(lat))) * ((n - s) / z.shape[0] * 110.54)).sum()
    return z, ext, mask, area

def main():
    style()
    z, ext, mask, area = highstand()
    print(f"highstand area {area:,.0f} km2 (USGS: 22,800)")
    w, e, s, n = ext
    k = np.cos(np.radians(40))
    hs = hillshade(z, ext, exag=1.5)
    base = np.array(to_rgb(CREAM)); dark = np.array(to_rgb("#8f8576"))
    rgb = base * (0.35 + 0.65 * hs[..., None]) + dark * 0 
    rgb = np.clip(rgb * 1.08, 0, 1)
    lake = np.array(to_rgb("#9ec3d6"))
    rgb[mask] = rgb[mask] * 0.35 + lake * 0.65
    feats = osm("pyramid")
    fig, ax = plt.subplots(figsize=(8.6, 8.4))
    ax.imshow(rgb, extent=(w * k, e * k, s, n), interpolation="lanczos", zorder=0)
    ax.contour(np.linspace(w, e, z.shape[1]) * k, np.linspace(n, s, z.shape[0]), mask.astype(float), [0.5],
               colors=["#3f7896"], linewidths=0.7, zorder=2)
    for name, near in [("Pyramid Lake", (-119.55, 40.0)), ("Walker Lake", (-118.7, 38.7)), ("Lake Tahoe", (-120.04, 39.09)),
                       ("Honey Lake", (-120.3, 40.25)), ("Lahontan Reservoir", (-119.1, 39.4))]:
        draw(ax, polygon(feats, name, near), k, color="#2f78a3", ec="none", zorder=4)
    riv = lines(feats, {"Truckee River", "Carson River", "Walker River", "West Walker River", "Humboldt River", "Susan River", "Quinn River"})
    draw(ax, riv, k, color="#3f86b0", lw=0.8, zorder=3)
    lab = [("Pyramid Lake", -119.50, 40.06, "left", INK, "medium"), ("Walker Lake", -118.66, 38.60, "left", INK, "medium"),
           ("Lake Tahoe", -120.12, 39.10, "right", INK, "medium"), ("Honey Lake", -120.20, 40.30, "right", INK, "medium"),
           ("Winnemucca Lake\n(dry since the 1930s)", -119.30, 40.42, "center", INK, "normal"),
           ("Black Rock\nDesert", -119.05, 41.00, "center", "#1f4f68", "normal"), ("Carson\nSink", -118.45, 39.82, "center", "#1f4f68", "normal"),
           ("Humboldt River", -117.6, 40.88, "center", "#1f4f68", "normal"), ("Truckee River", -119.95, 39.62, "right", "#1f4f68", "normal")]
    for t, lo, la, ha, col, wt in lab:
        ax.text(lo * k, la, t, ha=ha, va="center", fontsize=8.4, color=col, fontweight=wt, style="italic" if col != INK else "normal",
                bbox=dict(boxstyle="round,pad=0.12", fc=CREAM, ec="none", alpha=0.6) if col == INK else None, zorder=6)
    for name, lo, la in [("Reno", -119.81, 39.53), ("Fallon", -118.78, 39.47), ("Lovelock", -118.47, 40.18)]:
        ax.plot(lo * k, la, "o", ms=3, color=INK, zorder=6)
        ax.text(lo * k + 0.04, la - 0.03, name, fontsize=8, color=INK, ha="left", va="top", zorder=6)
    ax.set_xlim((w + 0.05) * k, (e - 0.05) * k); ax.set_ylim(s + 0.6, n - 0.25)
    ax.set_aspect("equal"); ax.axis("off")
    ax.text(0, 1.06, "Lake Lahontan, 15,000 years ago", transform=ax.transAxes, fontsize=14, color=INK, fontweight="medium")
    ax.text(0, 1.025, f"Today's terrain flooded to the ancient shoreline at {HIGH:,} m. Dark blue: the lakes that are left.",
            transform=ax.transAxes, fontsize=9, color=STONE)
    ax.text(1, -0.012, "Elevation: Copernicus GLO-90 · Water: © OpenStreetMap contributors · Highstand: USGS FS 2004-3044 · " + CREDIT,
            transform=ax.transAxes, fontsize=6.6, color=STONE, ha="right", va="top")
    fig.savefig(OUT / "lahontan.png", dpi=200, bbox_inches="tight")

if __name__ == "__main__":
    main()
