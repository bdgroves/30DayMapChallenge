"""fetch — cached downloads for the day scripts.

    import fetch
    raw = fetch.get(url, DATA / "thing.csv")          # downloads once, then reads the cache
    states = fetch.shapes(url, DATA / "states.zip")   # a zipped shapefile as a GeoDataFrame
    js = fetch.json_get(url, params)                  # one JSON request, retried, not cached

Every request carries the project's User-Agent and is retried a few times, because the
public services these maps use (USGS, Census, GBIF, EarthScope) all have bad minutes.
"""
from __future__ import annotations

import json
import time
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path

UA = {"User-Agent": "30DayMapChallenge-2026 (github.com/bdgroves/30DayMapChallenge)"}
STATES = "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_state_500k.zip"
COUNTRIES = "https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_0_countries.zip"
LAND = "https://naciscdn.org/naturalearth/10m/physical/ne_10m_land.zip"
LAKES = "https://naciscdn.org/naturalearth/10m/physical/ne_10m_lakes.zip"


def _open(url: str, timeout: int, tries: int) -> bytes:
    last = None
    for i in range(tries):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=timeout) as r:
                return r.read()
        except Exception as e:  # noqa: BLE001
            last = e
            time.sleep(4 * (i + 1))
    raise RuntimeError(f"{url[:120]}: {last}")


def get(url: str, path: Path, timeout: int = 600, tries: int = 4) -> bytes:
    """Download url to path once; later calls read the file."""
    path = Path(path)
    if not path.exists():
        path.parent.mkdir(parents=True, exist_ok=True)
        print(f"  downloading {path.name}", flush=True)
        tmp = path.with_suffix(path.suffix + ".part")
        tmp.write_bytes(_open(url, timeout, tries))
        tmp.replace(path)
    return path.read_bytes()


def json_get(url: str, params: dict | None = None, timeout: int = 120, tries: int = 4):
    if params:
        url += ("&" if "?" in url else "?") + urllib.parse.urlencode(params)
    for i in range(tries):                     # a reply cut off mid-stream is retried too
        try:
            return json.loads(_open(url, timeout, tries))
        except json.JSONDecodeError:
            if i == tries - 1:
                raise
            time.sleep(4 * (i + 1))


def shapes(url: str, path: Path):
    """A zipped shapefile (downloaded once) as a GeoDataFrame."""
    import geopandas as gpd
    path = Path(path)
    get(url, path)
    folder = path.with_suffix("")
    if not folder.exists():
        zipfile.ZipFile(path).extractall(folder)
    shp = next(folder.rglob("*.shp"))
    return gpd.read_file(shp)
