# GBIF downloads

Citable GBIF downloads, each with its own DOI, for any creature or plant, and a compact copy of the
records to map from. Same idea as Day 20's kangaroo rats, made reusable.

1. Add a request to `requests/`, e.g. `requests/ochotona-princeps.json`:

   ```json
   {"name": "Ochotona princeps", "rank": "SPECIES", "kingdom": "Animalia", "elevation": true}
   ```

2. Push it (or **Actions → GBIF downloads → Run workflow**). The Action signs in with the
   `GBIF_USER` / `GBIF_PWD` / `GBIF_EMAIL` secrets, asks GBIF for every record with coordinates and
   no geospatial issues, and waits for the DOI (usually 5-30 minutes).
3. It commits:
   - `downloads/<name>.json`: download key, DOI, record count, date and the citation to use;
   - `data/<name>.csv.gz`: position, coordinate uncertainty, date, kind of record (specimen,
     sighting...), recorded elevation, country, state, institution, and with `"elevation": true`
     the ground elevation at each record from the Copernicus GLO-30 DEM (`dem_m`).

Requests that already have a download are skipped. Delete `downloads/<name>.json` and
`data/<name>.csv.gz` to ask GBIF for a fresh one.

| Request | Common name | Why |
|---|---|---|
| ochotona-princeps | American pika | Are pikas being found higher up than they used to be? |
| oncorhynchus-mykiss-aguabonita | California golden trout | Native to two Kern Plateau streams, stocked across the West |
| oncorhynchus-mykiss-whitei | Little Kern golden trout | Threatened sister lineage in the Little Kern basin |
| oncorhynchus-clarkii-henshawi | Lahontan cutthroat trout | Pyramid Lake's giant trout, brought back from Pilot Peak |
| chasmistes-cujus | Cui-ui | Endangered lake sucker found only in Pyramid Lake |
| siphateles-bicolor | Tui chub | Pyramid Lake's most common fish |
| catostomus-tahoensis | Tahoe sucker | Lahontan basin native |
| richardsonius-egregius | Lahontan redside | Lahontan basin native minnow |

## Analyses

- [`pika/`](pika/): do the pre-1950 museum localities for the American pika still have pikas near
  them? Written up as [What's Up With the Pikas?](https://brooksgroves.com/blog/whats-up-with-the-pikas.html)
