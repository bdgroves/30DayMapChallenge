"""
Pikas, part 2: the 21 Washington pika sites from before 1950 with no GBIF record within 3 km since 2000.

Each site gets a group and a short note, from three checks made in October 2026:
  1. the GBIF download behind part 1 (doi:10.15468/dl.p7sdx7): modern records near the old pin,
  2. iNaturalist's API at every quality grade, not just the research-grade records that reach GBIF
     (gbif/pika/out/inat_wa_sites.json, pulled by a GitHub Action on 2026-10-20 UTC),
  3. the labels themselves, read against Dalquest's Mammals of Washington (1948) and place-name
     gazetteers, to see whether the pin is where the collector meant.

    python gbif/pika/part2/build.py   -> part2/sites.json, sites.csv, sites.gpx

Used for the October 20 update to blog/pikas-part-2-washington.html (the post's map data is
blog/data/pika-wa-sites.json in the site repo; its GPX is the eight sites still worth a look).
"""
import json, csv
from pathlib import Path
import numpy as np, pandas as pd

HERE = Path(__file__).resolve().parent
PIKA = HERE.parent
R = 6371.0

GROUPS = {
    "pinned": "Pinned in the wrong place",
    "close": "Just past the circle",
    "low": "The low sites",
    "gap": "Real gaps: go look",
}

# label in wa_sites_v2.json -> curated fields. look = where to look, when the pin isn't it.
NOTES = {
 "Austin Pass, Mt. Baker; Mt. Baker; Mt. Baker Lodge; Mt. Baker Lookout": dict(
    name="Mt. Baker: Austin Pass and the lodge", group="pinned", look=(48.848, -121.690),
    who="Conner Museum, WSU (5 specimens)",
    note="Five specimens labelled Mt. Baker, Mt. Baker Lodge, Austin Pass and Mt. Baker Lookout share one pin about 12 km south-west of Austin Pass. At Austin Pass itself, by the Heather Meadows road, GBIF has 61 pika records since 2000 within 3 km. Pikas are doing fine here.",
    go="Artist Point road, Heather Meadows. Easy."),
 "James Lake, Mt. Rainier; Mt. Rainier; Spray Park, Mt. Rainier": dict(
    name="Mt. Rainier: Spray Park and Lake James", group="pinned", look=(46.925, -121.825),
    who="Conner Museum, WSU (4 specimens, 1919)",
    note="The four 1919 specimens are pinned to Rainier's summit, where no pika lives (part 1 used the collector's label elevations instead). The labels name Spray Park and Lake James, and both have pikas on record since 2000 within 3 km.",
    go="Spray Park trail from Mowich Lake. Busy, beautiful, pikas in the talus."),
 "Mt. Adams": dict(
    name="Mt. Adams (summit pin)", group="pinned",
    who="Conner Museum, WSU (1 specimen, 1917)",
    note="Labelled just “Mt. Adams” and pinned to the summit at 3,738 m. The label doesn't say where on the mountain it was taken. Pikas are recorded on the south-east side today, around Bird Creek Meadows.",
    go="No way to know where; Bird Creek Meadows is the nearest known colony."),
 "Glacier Peak": dict(
    name="Glacier Peak", group="pinned",
    who="Conner Museum, WSU (1 specimen, 1924, label 1,768 m)",
    note="Pinned about 95 km north of the volcano, on Mount Spickard, which was called Glacier Peak until 1963 (part 2). Either mountain is possible; the label doesn't say.",
    go="Remote either way. Worth a museum check of the original label."),
 "Rock Creek Pass, Round Top Mt.": dict(
    name="Round Top Mountain, Selkirks", group="pinned", look=(48.8102, -117.1302),
    who="Walter Dalquest, MVZ (14 specimens, 1937–38)",
    note="Georeferenced in 2003 to a 4,465-ft Round Top in Spokane County, a record marked unverified. Dalquest's own book puts his Round Top in Pend Oreille County at about 6,000 ft, and there is a 6,453-ft Round Top Mountain in the Selkirks, 105 km north. A GBIF record from 2023 sits 1.4 km from that summit, and someone photographed a pika on the Shedroof Divide Trail there in September 2026.",
    go="Shedroof Divide Trail, Colville National Forest. A long day."),
 "Bird Creek Meadows, Mount Adams": dict(
    name="Bird Creek Meadows, Mt. Adams", group="close",
    who="Ernest Booth, UWBM (1 specimen, 1941)",
    note="GBIF's nearest record since 2000 is 3.6 km away, just past the circle, but iNaturalist has five pika observations within 3 km, including research-grade photos from 2018, 2023 and 2024 about 2 km west of the pin. Re-found.",
    go="Bird Creek Meadows, Yakama Nation Tract D (check access and fees)."),
 "Windy Peak": dict(
    name="Windy Peak, Pasayten", group="close",
    who="Leonard Wing 1940; G. E. Hudson 1947 (3 specimens)",
    note="Two records from 2022, 3.9 and 4.1 km away and a few hundred metres lower than the summit pin. Pikas are almost certainly still on this mountain; nobody has posted one from the top.",
    go="Pasayten Wilderness. A long trail day."),
 "Bagley Creek, Mt. Herman": dict(
    name="Bagley Creek, Mt. Herman", group="close",
    who="Conner Museum, WSU (1 specimen, 1928)",
    note="Right next door to the busiest pika country in the state. GBIF has 16 records since 2000 within 5 km and 89 within 10, around Bagley Lakes and Heather Meadows; iNaturalist has one within 3 km.",
    go="Bagley Lakes loop, Heather Meadows. Easy."),
 "Table Mountain, Wenatchee National Forest": dict(
    name="Table Mountain, Wenatchee NF", group="close",
    who="M. L. Johnson, PSM (2 specimens, 1949)",
    note="Records from 2022 sit 4.8 to 5 km away. Close enough that this one is probably about where people walk, not about the pikas.",
    go="Forest roads north of Ellensburg, around Haney Meadow."),
 "NACHES PASS": dict(
    name="Naches Pass", group="close",
    who="Ernest Booth (1 specimen, 1940)",
    note="The nearest GBIF record since 2000 is 5.9 km away, and iNaturalist has 14 pika observations within 10 km. The old wagon-road pass gets few visitors compared with Chinook Pass next door.",
    go="The old wagon route over the crest. Rough forest roads."),
 "Keechelus": dict(
    name="Lake Keechelus", group="low",
    who="George Cantwell 1917 (YPM); D. E. Brown 1919 and Walter Dalquest 1940 (MVZ)",
    note="The type locality: the specimen that named the Cascade pika subspecies, Ochotona princeps brunnescens, was taken here by George Cantwell on 23 August 1917. Today Interstate 90 runs along the lake. The site sits at about 765 m, and the nearest record since 2000 is 6 km away and about 500 m higher, though pikas are everywhere around Snoqualmie Pass.",
    go="Talus along the lake and I-90 corridor. Stay safe off the freeway."),
 "Tye": dict(
    name="Tye (old Wellington)", group="pinned", look=(47.7493, -121.1207),
    who="UWBM (1 specimen, 1939)",
    note="Pinned where the Tye River meets the Foss, at 341 m. In 1939 \u201cTye\u201d was the railroad town below Stevens Pass that had been called Wellington until the 1910 avalanche, 14 km east and at 3,257 ft. GBIF has a pika record from 2026 about 300 m from the old townsite.",
    go="Wellington ghost town and the Iron Goat Trail, off US 2. Easy."),
 "Bumping River, Goose Prarie": dict(
    name="Goose Prairie, Bumping River", group="low",
    who="Conner Museum, WSU (1 specimen, 1917)",
    note="At 778 m on the dry east side of the Cascades. The nearest record since 2000 is 6.2 km away and 300 m higher, and there's only one within 10 km.",
    go="Bumping River road, west of SR 410."),
 "NACHREES HWY, NEAR; MILK CREEK": dict(
    name="Milk Creek, Naches Highway", group="low",
    who="Walter Dalquest (5 specimens, 1940)",
    note="Dalquest singled this place out: Milk Creek was the driest, warmest life zone where he found pikas in Washington. At roughly 800 m, it's exactly the kind of low, warm site that part 1 found missing most often. Nothing on record within 3 km since 2000.",
    go="SR 410 corridor near Milk Creek. Rock slides by the highway."),
 "Twin Lakes": dict(
    name="Twin Lakes (near Lake Wenatchee)", group="low",
    who="Conner Museum, WSU (1 specimen, 1917)",
    note="The pin is at 871 m. The nearest record since 2000 is 4.5 km away but about 900 m higher, up on the ridges.",
    go="Lake Wenatchee area. Which Twin Lakes is uncertain."),
 "Stehekin": dict(
    name="Stehekin", group="low",
    who="Conner Museum, WSU (1 specimen, 1918)",
    note="Labelled with the village at the head of Lake Chelan, 371 m. The animal probably came from the talus somewhere above. No pika has been posted within 10 km, but Stehekin is reached only by boat, plane or a long hike.",
    go="Talus along the Stehekin valley road and Rainbow Loop."),
 "Loomis": dict(
    name="Loomis", group="low",
    who="Stanley Jewett (1 specimen, 1918)",
    note="Labelled with a town at 397 m in the Okanogan. Collectors often wrote the nearest post office, so this pika probably came from the hills around it, maybe the high country to the west. The nearest record since 2000 is 26 km away.",
    go="Okanogan highlands west of Loomis; Chopaka and the Pasayten edge."),
 "Sherman Creek Pass; Sherman Pass": dict(
    name="Sherman Pass, Kettle River Range", group="gap",
    who="Walter Dalquest 1938 (MVZ); Ernest Booth 1944; Clanton 1947 (8 specimens)",
    note="The biggest gap on the map. Dalquest took young pikas of several sizes here on 11 September 1938, so it was a breeding colony. Since 2000 there's no GBIF pika record within 82 km and no iNaturalist observation of any grade within 10 km. Sherman Pass is on Highway 20 and the Kettle Crest Trail starts right there.",
    go="Kettle Crest Trail from Sherman Pass, SR 20. Easy access; listen for the eep."),
 "16MI N CONCONULLY,FUNK MT,4000FT": dict(
    name="Funk Mountain, Okanogan", group="gap",
    who="Leonard Wing (1 specimen, 1940)",
    note="No pika on record within 10 km since 2000, on GBIF or iNaturalist. The nearest is 14 km away. There's a fire lookout on Funk Mountain, so people do go up.",
    go="Funk Mountain lookout, north-west of Conconully."),
 "Mount Higgins; Lookout Station": dict(
    name="Mount Higgins lookout", group="gap",
    who="UWBM (2 specimens, 1940)",
    note="Taken at the old lookout station above the North Fork Stillaguamish. Nothing within 10 km since 2000; the nearest GBIF record is 13.6 km away.",
    go="Mount Higgins trail off SR 530. Steep."),
 "Whatcom Pass": dict(
    name="Whatcom Pass", group="gap",
    who="J. M. Edson, 1920 (3 specimens)",
    note="Deep in North Cascades National Park, a multi-day trip each way. One pika record within 10 km, from 2014. Probably a gap in hikers more than in pikas, but nobody knows.",
    go="Whatcom Pass via Hannegan Pass and the Chilliwack. Backpack."),
}


def hav(lat, lon, la, lo):
    p = np.radians
    a = np.sin(p(la - lat) / 2) ** 2 + np.cos(p(lat)) * np.cos(p(la)) * np.sin(p(lo - lon) / 2) ** 2
    return 2 * R * np.arcsin(np.sqrt(a))


def main():
    sites = [s for s in json.loads((PIKA / "out" / "wa_sites_v2.json").read_text()) if not s["refound"]]
    inat = {r["label"]: r for r in json.loads((PIKA / "out" / "inat_wa_sites.json").read_text())}
    assert len(sites) == 21 and set(NOTES) == {s["label"] for s in sites}, "labels changed"
    out = []
    for s in sites:
        n = NOTES[s["label"]]
        i = inat.get(s["label"], {})
        r = dict(name=n["name"], group=n["group"], group_name=GROUPS[n["group"]], label=s["label"],
                 years=s["years"], specimens=s["specimens"], who=n["who"], catalog=s["catalog"],
                 lat=s["lat"], lon=s["lon"], elev_m=s["elev_m"],
                 look=list(n["look"]) if n.get("look") else None,
                 gbif_nearest_km=s["nearest_km"], gbif_nearest_year=s["nearest_year"],
                 inat_3km=i.get("n3"), inat_10km=i.get("n10"), note=n["note"], go=n["go"])
        out.append(r)
    order = list(GROUPS)
    out.sort(key=lambda r: (order.index(r["group"]), -r["lat"]))
    (HERE / "sites.json").write_text(json.dumps(out, indent=1, ensure_ascii=False))
    with open(HERE / "sites.csv", "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["group", "name", "label_on_specimen", "years", "specimens", "collected_by", "lat", "lon",
                    "look_lat", "look_lon", "elev_m", "gbif_nearest_km_since_2000", "inat_obs_within_3km",
                    "inat_obs_within_10km", "note", "how_to_get_there", "catalog_numbers"])
        for r in out:
            lk = r["look"] or [None, None]
            w.writerow([r["group_name"], r["name"], r["label"], r["years"], r["specimens"], r["who"], r["lat"], r["lon"],
                        lk[0], lk[1], r["elev_m"], r["gbif_nearest_km"], r["inat_3km"], r["inat_10km"], r["note"], r["go"],
                        "; ".join(r["catalog"])])
    esc = lambda t: t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    g = ['<?xml version="1.0" encoding="UTF-8"?>',
         '<gpx version="1.1" creator="brooksgroves.com pikas part 2" xmlns="http://www.topografix.com/GPX/1/1">',
         '<metadata><name>Washington\'s missing pika sites (pre-1950, no GBIF record within 3 km since 2000)</name>'
         '<link href="https://brooksgroves.com/blog/pikas-part-2-washington.html"><text>Pikas, part 2</text></link></metadata>']
    for r in out:
        la, lo = r["look"] or (r["lat"], r["lon"])
        g.append(f'<wpt lat="{la:.5f}" lon="{lo:.5f}"><name>{esc(r["name"])}</name>'
                 f'<desc>{esc(r["group_name"] + ". " + r["note"] + " Getting there: " + r["go"])}</desc>'
                 f'<type>{esc(r["group_name"])}</type></wpt>')
    g.append("</gpx>")
    (HERE / "sites.gpx").write_text("\n".join(g))
    print(len(out), "sites;", {k: sum(r["group"] == k for r in out) for k in GROUPS})


if __name__ == "__main__":
    main()
