"""Five native fishes of the Lahontan basin, each mapped over the Lake Lahontan highstand.
Writes out/natives.png and prints each species' record counts."""
import sys
from pathlib import Path
import numpy as np
import pandas as pd
from matplotlib.colors import to_rgb
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from mapkit import *   # noqa: F403
from lake import highstand

HERE = Path(__file__).resolve().parent
D = HERE.parent / "data"
SP = [("chasmistes-cujus", "Cui-ui", "Chasmistes cujus"),
      ("oncorhynchus-clarkii-henshawi", "Lahontan cutthroat trout", "Oncorhynchus clarkii henshawi"),
      ("siphateles-bicolor", "Tui chub", "Siphateles bicolor"),
      ("catostomus-tahoensis", "Tahoe sucker", "Catostomus tahoensis"),
      ("richardsonius-egregius", "Lahontan redside", "Richardsonius egregius")]

def main():
    style()
    z, ext, mask, _ = highstand()
    w, e, s, n = ext
    k = np.cos(np.radians(40))
    hs = hillshade(z[::3, ::3], ext, exag=1.5)
    m3 = mask[::3, ::3]
    base = np.array(to_rgb(CREAM))
    rgb = np.clip(base * (0.5 + 0.55 * hs[..., None]), 0, 1)
    rgb[m3] = rgb[m3] * 0.35 + np.array(to_rgb("#a9cbdc")) * 0.65
    feats = osm("pyramid")
    lakes = [polygon(feats, nm, near) for nm, near in [("Pyramid Lake", (-119.55, 40.0)), ("Walker Lake", (-118.7, 38.7)),
             ("Lake Tahoe", (-120.04, 39.09)), ("Honey Lake", (-120.3, 40.25))]]
    fig, axs = plt.subplots(2, 3, figsize=(11, 8.4))
    rows = []
    for ax, (slug, common, sci) in zip(axs.flat, SP):
        d = pd.read_csv(D / f"{slug}.csv.gz", low_memory=False)
        d = d[d.lon.between(w, e) & d.lat.between(s, n)]
        r = ((n - d.lat) / (n - s) * mask.shape[0]).astype(int).clip(0, mask.shape[0] - 1)
        c = ((d.lon - w) / (e - w) * mask.shape[1]).astype(int).clip(0, mask.shape[1] - 1)
        inside = mask[r.values, c.values].mean()
        rows.append((common, len(d), inside, int(d.year.min()), int(d.year.max())))
        ax.imshow(rgb, extent=(w * k, e * k, s, n), interpolation="lanczos")
        for p in lakes:
            draw(ax, p, k, color="#2f78a3", ec="none")
        ax.scatter(d.lon * k, d.lat, s=8, color=RUST, ec=PAPER, lw=0.35, zorder=5)
        ax.set_xlim((w + 0.2) * k, (e - 0.3) * k); ax.set_ylim(s + 0.7, n - 0.3)
        ax.set_aspect("equal"); ax.axis("off")
        ax.set_title(f"{common}", loc="left", fontsize=11.5, color=INK, fontweight="medium", pad=16)
        ax.text(0, 1.012, f"{sci} · {len(d):,} records · {inside:.0%} inside the old lake", transform=ax.transAxes,
                fontsize=7.6, color=STONE, style="italic")
    ax = axs.flat[5]; ax.axis("off")
    ax.text(0.02, 0.78, "How to read these", fontsize=11.5, color=INK, fontweight="medium", transform=ax.transAxes)
    ax.text(0.02, 0.70, "Pale blue: Lake Lahontan at its highstand,\nabout 15,000 years ago (1,335 m).\n\n"
            "Dark blue: Pyramid Lake, Walker Lake,\nLake Tahoe and Honey Lake today.\n\n"
            "Rust dots: every GBIF record in this\nwindow, museum specimens and\niNaturalist sightings, 1853 to today.",
            fontsize=9, color=STONE, va="top", transform=ax.transAxes, linespacing=1.45)
    fig.text(0.01, 1.0, "Five fish of an inland sea", fontsize=15, color=INK, fontweight="medium", va="bottom")
    fig.text(0.01, 0.975, "Native fishes of the Lahontan basin, mapped over the ice-age lake that connected their homes",
             fontsize=9.5, color=STONE, va="bottom")
    fig.text(0.99, 0.0, "Records: GBIF.org (5 October 2026), DOIs in the post · Elevation: Copernicus GLO-90 · Lakes: © OpenStreetMap contributors · " + CREDIT,
             fontsize=6.6, color=STONE, ha="right", va="top")
    fig.subplots_adjust(wspace=0.04, hspace=0.12, left=0.01, right=0.99, top=0.93, bottom=0.02)
    fig.savefig(HERE / "out" / "natives.png", dpi=200, bbox_inches="tight")
    for r in rows:
        print(*r, sep="\t")

if __name__ == "__main__":
    main()
