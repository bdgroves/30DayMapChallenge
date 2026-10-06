"""
Washington pika records with their labels: where the collector said they were, who, and the
catalogue number, from the same citable download (doi:10.15468/dl.p7sdx7). For the part 2
field checklist of pre-1950 sites with no record since 2000.

    python gbif/pika/details.py   -> gbif/pika/out/wa_records.csv
(Run by the "Pika details" Action: it needs the GBIF API.)
"""
import io, json, zipfile, urllib.request
from pathlib import Path
import pandas as pd

HERE = Path(__file__).resolve().parent
info = json.loads((HERE.parent / "downloads" / "ochotona-princeps.json").read_text())
KEEP = ["gbifID", "occurrenceID", "basisOfRecord", "institutionCode", "collectionCode", "catalogNumber",
        "recordedBy", "eventDate", "year", "locality", "stateProvince", "decimalLatitude", "decimalLongitude",
        "coordinateUncertaintyInMeters", "elevation", "elevationAccuracy", "datasetKey", "issue"]
req = urllib.request.Request(info["link"], headers={"User-Agent": "bdgroves GBIF maps (brooksgroves.com)"})
raw = urllib.request.urlopen(req, timeout=900).read()
with zipfile.ZipFile(io.BytesIO(raw)) as zf:
    name = next(n for n in zf.namelist() if n.endswith(".csv"))
    with zf.open(name) as fh:
        df = pd.read_csv(fh, sep="\t", usecols=lambda c: c in KEEP, quoting=3, on_bad_lines="skip",
                         low_memory=False, dtype=str)
wa = df[df.stateProvince.fillna("").str.contains("Washington", case=False)]
(HERE / "out").mkdir(exist_ok=True)
wa.to_csv(HERE / "out" / "wa_records.csv", index=False)
print(f"= {len(wa)} Washington records of {len(df)} (columns: {', '.join(wa.columns)})")
