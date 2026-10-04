<!-- plan:start (generated from days.yml; edit there) -->
# Day 29 · Raster

**Sunday, November 29, 2026** · status: ✏️ draft

## When Rainier's snow melted

Every 30 m patch of Mount Rainier coloured by the day it lost its snow in 2026, from Sentinel-2's scene classification, with the glaciers that never melted in white and Paradise's SNOTEL record from my snowpack tracker as ground truth.

**Data**

- [Sentinel-2 L2A scene classification (Earth Search, AWS open data)](https://earth-search.aws.element84.com/v1)
- [NRCS SNOTEL via STORM CHASER, my Rainier snowpack tracker](https://brooksgroves.com/rainier-snowpack)

**Tools:** Python, rasterio, STAC

**Prep:** Drafted. No login needed: Sentinel-2 comes from AWS open data, SNOTEL from the tracker's own archive.

**Go deeper**

- [STORM CHASER: Rainier snowpack, live](https://brooksgroves.com/rainier-snowpack)
- [CASCADIA-WX: Northwest weather balloons](https://brooksgroves.com/cascadia-wx/)

Folder: `data/` for downloads (not committed), `out/` for the finished map (`map.png`, `alt.txt`).
<!-- plan:end -->

## Notes

