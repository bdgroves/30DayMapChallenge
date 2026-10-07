<!-- plan:start (generated from days.yml; edit there) -->
# Day 9 · Urban-rural

**Monday, November 9, 2026** · status: ✏️ draft

## Tacoma to Paradise

One line from the Port of Tacoma to Paradise on Rainier, and what changes along it: people, buildings, trees and elevation.

**Data**

- [Census 2020 blocks and population](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
- [GHSL built-up surface](https://human-settlement.emergency.copernicus.eu/)

**Tools:** Python, rasterio, matplotlib

**Prep:** Drafted (2020 census blocks, Copernicus DEM).

Folder: `data/` for downloads (not committed), `out/` for the finished map (`map.png`, `alt.txt`).
<!-- plan:end -->

## Notes

- The line, laid flat: a 5 km-wide Sentinel-2 ribbon (8 July 2025), north-east side up, with stacked profiles on the same km scale for people, buildings, tree cover and ground. Counts are for the middle 2 km.
- Zones are set from the profiles (see `ZONES` in make.py) and named after Patrick Geddes's Valley Section: fisher, the city, peasant, woodman and hunter, miner.
- The red is the "Lahars" class of the USGS simplified volcanic hazards (WA DNR service), 32 km of the 74. The near-volcano zone is left off.
- `python make.py --draw` redraws from `out/profiles.csv`, `out/elevation.csv`, `out/ribbon.jpg` and `out/lahar_ribbon.geojson` without any downloads.
- Fact-checked by a separate agent (Oct 2026): "west flank" corrected to south-west; data credits expanded.
