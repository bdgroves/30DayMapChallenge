"""
Day 20: request the citable GBIF download (with its DOI) for the kangaroo rat map.

Run by the "GBIF download for Day 20" GitHub Action, which passes the account in as secrets
(GBIF_USER, GBIF_PWD, GBIF_EMAIL; set them in the repo's Settings -> Secrets and variables ->
Actions). It asks GBIF for every georeferenced Dipodomys record, with the same filters the draft
used, waits for GBIF to prepare it (usually 5-20 minutes), and writes gbif_download.json beside
this file: the download key, DOI, record count and date. make.py then builds the map from that
exact download and cites its DOI.

If gbif_download.json already exists, nothing new is requested (delete it to ask for a fresh one).
The download also appears under Downloads on the account's gbif.org profile.

    python day-20-hexagons/gbif_download.py
"""
import base64
import json
import os
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUT = HERE / "gbif_download.json"
API = "https://api.gbif.org/v1"


def call(url, data=None, auth=None, tries=5):
    for i in range(tries):
        req = urllib.request.Request(url, data=data, headers={"User-Agent": "30DayMapChallenge (brooksgroves.com)"})
        if data is not None:
            req.add_header("Content-Type", "application/json")
        if auth:
            req.add_header("Authorization", "Basic " + base64.b64encode(auth.encode()).decode())
        try:
            with urllib.request.urlopen(req, timeout=120) as r:
                return r.read().decode()
        except urllib.error.HTTPError as e:
            if e.code in (401, 403):
                sys.exit(f"GBIF refused the login ({e.code}). Check the GBIF_USER and GBIF_PWD secrets.")
            if i == tries - 1:
                raise
        except (urllib.error.URLError, TimeoutError):
            if i == tries - 1:
                raise
        time.sleep(10 * (i + 1))


def main():
    if OUT.exists():
        print(f"Already have a download: {json.loads(OUT.read_text())['doi']}")
        return
    user, pwd, email = (os.environ.get(k, "").strip() for k in ("GBIF_USER", "GBIF_PWD", "GBIF_EMAIL"))
    if not (user and pwd):
        sys.exit("No GBIF account: add the GBIF_USER, GBIF_PWD and GBIF_EMAIL secrets to the repo, then run again.")
    q = urllib.parse.urlencode(dict(name="Dipodomys", rank="GENUS", kingdom="Animalia"))
    key = json.loads(call(f"{API}/species/match?{q}"))["usageKey"]
    body = {
        "creator": user,
        "notificationAddresses": [email] if email else [],
        "sendNotification": bool(email),
        "format": "SIMPLE_CSV",
        "predicate": {"type": "and", "predicates": [
            {"type": "equals", "key": "TAXON_KEY", "value": str(key)},
            {"type": "equals", "key": "HAS_COORDINATE", "value": "true"},
            {"type": "equals", "key": "HAS_GEOSPATIAL_ISSUE", "value": "false"},
            {"type": "equals", "key": "OCCURRENCE_STATUS", "value": "PRESENT"},
        ]},
    }
    dl = call(f"{API}/occurrence/download/request", json.dumps(body).encode(), auth=f"{user}:{pwd}").strip()
    print(f"Requested GBIF download {dl} (genus key {key}); waiting for GBIF to prepare it...", flush=True)
    for _ in range(240):                                  # up to 2 hours
        st = json.loads(call(f"{API}/occurrence/download/{dl}"))
        if st["status"] in ("SUCCEEDED", "FAILED", "KILLED", "CANCELLED"):
            break
        time.sleep(30)
    if st["status"] != "SUCCEEDED":
        sys.exit(f"GBIF download {dl} ended as {st['status']}")
    info = {"key": dl, "doi": st.get("doi"), "records": st.get("totalRecords"), "created": st.get("created", "")[:10],
            "link": st.get("downloadLink"), "genus_key": key}
    OUT.write_text(json.dumps(info, indent=2) + "\n")
    print(f"= GBIF download ready: https://doi.org/{info['doi']} ({info['records']:,} records)")


if __name__ == "__main__":
    main()
