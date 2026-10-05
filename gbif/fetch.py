"""
GBIF downloads with DOIs, for any creature or plant.

Each file in gbif/requests/ names a taxon, e.g. requests/ochotona-princeps.json:

    {"name": "Ochotona princeps", "rank": "SPECIES", "kingdom": "Animalia", "elevation": true}

For every request without a gbif/downloads/<slug>.json yet, this asks GBIF (with the account in the
GBIF_USER / GBIF_PWD / GBIF_EMAIL secrets) for every record of that taxon with coordinates, no
geospatial issues and status PRESENT, waits for the download and its DOI, and records the key, DOI,
count and date in downloads/<slug>.json. Then it writes data/<slug>.csv.gz, a compact copy of the
records for mapping (position, uncertainty, date, kind of record, recorded elevation, country and
state), plus, when the request says "elevation": true, the ground elevation at each record from the
Copernicus GLO-30 DEM.

Run by the "GBIF downloads" GitHub Action whenever a request is added or changed.

    python gbif/fetch.py
"""
import base64
import io
import json
import os
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
API = "https://api.gbif.org/v1"
COLS = {"decimalLatitude": "lat", "decimalLongitude": "lon", "coordinateUncertaintyInMeters": "uncert_m",
        "eventDate": "date", "year": "year", "month": "month", "basisOfRecord": "basis", "elevation": "elev_given_m",
        "countryCode": "country", "stateProvince": "state", "institutionCode": "institution", "species": "species"}


def call(url, data=None, auth=None, tries=5, raw=False):
    for i in range(tries):
        req = urllib.request.Request(url, data=data, headers={"User-Agent": "bdgroves GBIF maps (brooksgroves.com)"})
        if data is not None:
            req.add_header("Content-Type", "application/json")
        if auth:
            req.add_header("Authorization", "Basic " + base64.b64encode(auth.encode()).decode())
        try:
            with urllib.request.urlopen(req, timeout=600) as r:
                b = r.read()
                return b if raw else b.decode()
        except urllib.error.HTTPError as e:
            if e.code in (401, 403):
                sys.exit(f"GBIF refused the login ({e.code}). Check the GBIF_USER and GBIF_PWD secrets.")
            if i == tries - 1:
                raise
        except (urllib.error.URLError, TimeoutError):
            if i == tries - 1:
                raise
        time.sleep(10 * (i + 1))


def request(slug, req):
    out = HERE / "downloads" / f"{slug}.json"
    if out.exists():
        return json.loads(out.read_text())
    user, pwd, email = (os.environ.get(k, "").strip() for k in ("GBIF_USER", "GBIF_PWD", "GBIF_EMAIL"))
    if not (user and pwd):
        sys.exit("No GBIF account: add the GBIF_USER, GBIF_PWD and GBIF_EMAIL secrets to the repo.")
    q = urllib.parse.urlencode({k: req[k] for k in ("name", "rank", "kingdom") if k in req})
    match = json.loads(call(f"{API}/species/match?{q}"))
    if match.get("matchType") == "NONE":
        sys.exit(f"{slug}: GBIF doesn't recognise {req['name']}")
    key = match["usageKey"]
    body = {"creator": user, "notificationAddresses": [email] if email else [], "sendNotification": bool(email),
            "format": "SIMPLE_CSV",
            "predicate": {"type": "and", "predicates": [
                {"type": "equals", "key": "TAXON_KEY", "value": str(key)},
                {"type": "equals", "key": "HAS_COORDINATE", "value": "true"},
                {"type": "equals", "key": "HAS_GEOSPATIAL_ISSUE", "value": "false"},
                {"type": "equals", "key": "OCCURRENCE_STATUS", "value": "PRESENT"}]}}
    dl = call(f"{API}/occurrence/download/request", json.dumps(body).encode(), auth=f"{user}:{pwd}").strip()
    print(f"{slug}: requested GBIF download {dl} for {match.get('scientificName')} (key {key}); waiting...", flush=True)
    for _ in range(240):
        st = json.loads(call(f"{API}/occurrence/download/{dl}"))
        if st["status"] in ("SUCCEEDED", "FAILED", "KILLED", "CANCELLED"):
            break
        time.sleep(30)
    if st["status"] != "SUCCEEDED":
        sys.exit(f"{slug}: GBIF download {dl} ended as {st['status']}")
    info = {"name": req["name"], "taxon_key": key, "key": dl, "doi": st.get("doi"), "records": st.get("totalRecords"),
            "created": st.get("created", "")[:10], "link": st.get("downloadLink"),
            "citation": f"GBIF.org ({pd.Timestamp(st.get('created', '')[:10]):%-d %B %Y}) GBIF Occurrence Download "
                        f"https://doi.org/{st.get('doi')}"}
    out.write_text(json.dumps(info, indent=2) + "\n")
    print(f"= {slug}: {info['citation']} ({info['records']:,} records)", flush=True)
    return info


def ground_elevation(df):
    """Copernicus GLO-30 elevation at each record, one 1-degree tile at a time (read over HTTP)."""
    import rasterio
    elev = np.full(len(df), np.nan)
    env = dict(GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR", CPL_VSIL_CURL_ALLOWED_EXTENSIONS=".tif", VSI_CACHE="TRUE")
    tiles = df.groupby([np.floor(df["lat"]).astype(int), np.floor(df["lon"]).astype(int)]).indices
    with rasterio.Env(**env):
        for n, ((la, lo), idx) in enumerate(tiles.items(), 1):
            ns = f"N{la:02d}" if la >= 0 else f"S{-la:02d}"
            ew = f"E{lo:03d}" if lo >= 0 else f"W{-lo:03d}"
            name = f"Copernicus_DSM_COG_10_{ns}_00_{ew}_00_DEM"
            try:
                with rasterio.open(f"/vsicurl/https://copernicus-dem-30m.s3.amazonaws.com/{name}/{name}.tif") as src:
                    pts = list(zip(df["lon"].values[idx], df["lat"].values[idx]))
                    elev[idx] = [v[0] for v in src.sample(pts)]
            except Exception as e:                                   # noqa: BLE001 - ocean tiles don't exist
                print(f"  no DEM tile {name}: {type(e).__name__}")
            if n % 20 == 0:
                print(f"  elevation: {n}/{len(tiles)} tiles", flush=True)
    return np.where(elev < -500, np.nan, elev)


def records(slug, req, info):
    out = HERE / "data" / f"{slug}.csv.gz"
    if out.exists():
        return
    raw = call(info["link"] or f"{API}/occurrence/download/request/{info['key']}.zip", raw=True)
    with zipfile.ZipFile(io.BytesIO(raw)) as zf:
        name = next(n for n in zf.namelist() if n.endswith(".csv"))
        with zf.open(name) as fh:
            df = pd.read_csv(fh, sep="\t", usecols=lambda c: c in COLS, quoting=3, on_bad_lines="skip",
                             low_memory=False)
    df = df.rename(columns=COLS)
    df = df[df["lat"].between(-90, 90) & df["lon"].between(-180, 180)]
    df["date"] = df["date"].astype(str).str[:10]
    if req.get("elevation"):
        print(f"{slug}: ground elevation for {len(df):,} records", flush=True)
        df["dem_m"] = np.round(ground_elevation(df))
    df.to_csv(out, index=False, compression="gzip")
    print(f"= {slug}: wrote data/{slug}.csv.gz ({len(df):,} records)", flush=True)


def main():
    reqs = sorted((HERE / "requests").glob("*.json"))
    for p in reqs:
        slug = p.stem
        req = json.loads(p.read_text())
        info = request(slug, req)
        records(slug, req, info)


if __name__ == "__main__":
    main()
