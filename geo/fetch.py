"""
Terrain and named features for maps, fetched where the network is open (the "Geo data" GitHub Action).

Each file in geo/requests/ is one of:

    {"kind": "dem", "bounds": [W, S, E, N], "res": 0.0025, "source": "90"}   Copernicus GLO-90 (or "30")
    {"kind": "osm", "bounds": [W, S, E, N], "query": "<Overpass QL body using {bbox}>"}

and writes geo/data/<slug>.tif (int16 metres, deflate) or geo/data/<slug>.geojson. Existing outputs
are skipped; delete one to fetch it again.

    python geo/fetch.py
"""
import json
import math
import sys
import time
import urllib.parse
import urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent
OVERPASS = ["https://overpass-api.de/api/interpreter", "https://overpass.kumi.systems/api/interpreter"]


def dem(slug, req, out):
    import numpy as np
    import rasterio
    from rasterio.merge import merge
    w, s, e, n = req["bounds"]
    src = req.get("source", "90")
    code = "30" if src == "90" else "10"
    bucket = f"copernicus-dem-{src}m"
    urls = []
    for la in range(math.floor(s), math.ceil(n)):
        for lo in range(math.floor(w), math.ceil(e)):
            ns = f"N{la:02d}" if la >= 0 else f"S{-la:02d}"
            ew = f"E{lo:03d}" if lo >= 0 else f"W{-lo:03d}"
            name = f"Copernicus_DSM_COG_{code}_{ns}_00_{ew}_00_DEM"
            urls.append(f"/vsicurl/https://{bucket}.s3.amazonaws.com/{name}/{name}.tif")
    env = dict(GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR", CPL_VSIL_CURL_ALLOWED_EXTENSIONS=".tif", VSI_CACHE="TRUE",
               GDAL_HTTP_MAX_RETRY="4", GDAL_HTTP_RETRY_DELAY="3")
    with rasterio.Env(**env):
        srcs = []
        for u in urls:
            try:
                srcs.append(rasterio.open(u))
            except Exception as ex:                             # noqa: BLE001 - ocean tiles don't exist
                print(f"  {slug}: no tile {u.rsplit('/', 1)[-1]} ({type(ex).__name__})")
        print(f"{slug}: merging {len(srcs)} tiles at {req['res']} deg", flush=True)
        arr, tr = merge(srcs, bounds=(w, s, e, n), res=req["res"], nodata=-32768, resampling=rasterio.enums.Resampling.average)
    a = np.where(arr[0] < -1000, -32768, np.round(arr[0])).astype("int16")
    prof = dict(driver="GTiff", height=a.shape[0], width=a.shape[1], count=1, dtype="int16", crs="EPSG:4326",
                transform=tr, nodata=-32768, compress="deflate", predictor=2, tiled=True, blockxsize=256, blockysize=256)
    with rasterio.open(out, "w", **prof) as dst:
        dst.write(a, 1)
    print(f"= {slug}: {a.shape[1]}x{a.shape[0]} -> {out.name} ({out.stat().st_size/1e6:.1f} MB)", flush=True)


def osm(slug, req, out):
    w, s, e, n = req["bounds"]
    q = "[out:json][timeout:180];(" + req["query"].replace("{bbox}", f"{s},{w},{n},{e}") + ");out geom;"
    data = None
    for i in range(6):
        url = OVERPASS[i % len(OVERPASS)]
        try:
            r = urllib.request.urlopen(urllib.request.Request(url, data=urllib.parse.urlencode({"data": q}).encode(),
                                       headers={"User-Agent": "bdgroves maps (brooksgroves.com)"}), timeout=300)
            data = json.loads(r.read())
            break
        except Exception as ex:                                 # noqa: BLE001
            print(f"  {slug}: overpass try {i + 1} failed ({ex})", flush=True)
            time.sleep(20 * (i + 1))
    if data is None:
        raise SystemExit(f"{slug}: Overpass failed")
    feats = []
    for el in data["elements"]:
        tags = el.get("tags", {})
        if el["type"] == "node":
            geom = {"type": "Point", "coordinates": [el["lon"], el["lat"]]}
        elif el["type"] == "way":
            geom = {"type": "LineString", "coordinates": [[p["lon"], p["lat"]] for p in el.get("geometry", [])]}
        else:
            lines = [[[p["lon"], p["lat"]] for p in m["geometry"]] for m in el.get("members", [])
                     if m.get("type") == "way" and m.get("geometry") and m.get("role", "") in ("", "outer", "main_stream", "side_stream")]
            geom = {"type": "MultiLineString", "coordinates": lines}
        feats.append({"type": "Feature", "properties": {"osm": f"{el['type']}/{el['id']}", **tags}, "geometry": geom})
    out.write_text(json.dumps({"type": "FeatureCollection", "features": feats}))
    print(f"= {slug}: {len(feats)} features -> {out.name}", flush=True)


def main():
    (HERE / "data").mkdir(exist_ok=True)
    failed = []
    for p in sorted((HERE / "requests").glob("*.json")):
        req = json.loads(p.read_text())
        out = HERE / "data" / (p.stem + (".tif" if req["kind"] == "dem" else ".geojson"))
        if out.exists():
            continue
        try:
            (dem if req["kind"] == "dem" else osm)(p.stem, req, out)
        except BaseException as ex:                             # noqa: BLE001
            print(f"! {p.stem}: {ex}", flush=True)
            failed.append(p.stem)
    if failed:
        sys.exit(f"failed: {' '.join(failed)}")


if __name__ == "__main__":
    main()
