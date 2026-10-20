import json, time, urllib.request, urllib.parse
sites=[s for s in json.load(open('probe/wa_sites_v2.json')) if not s['refound']]
extra={'Round Top Mtn (Pend Oreille)':(48.8102,-117.1302),'Pass Creek Pass area':(48.83,-117.25),'Austin Pass':(48.848,-121.690),
 'Spray Park':(46.925,-121.825),'Lake James':(46.962,-121.780),'Glacier Peak':(48.112,-121.114),'Mt Spokane':(47.9219,-117.1131),
 'Funk Mountain lookout':(48.62,-119.77),'Kettle Crest (Copper Butte)':(48.70,-118.47),'Lake Keechelus N end':(47.39,-121.39)}
pts=[(s['label'],s['lat'],s['lon']) for s in sites]+[(k,a,b) for k,(a,b) in extra.items()]
out=[]
for lab,la,lo in pts:
    res={'label':lab,'lat':la,'lon':lo}
    for r in (3,10):
        q=urllib.parse.urlencode(dict(taxon_id=43188,lat=la,lng=lo,radius=r,per_page=10,order_by='observed_on',quality_grade='any'))
        try:
            req=urllib.request.Request('https://api.inaturalist.org/v1/observations?'+q,headers={'User-Agent':'bdgroves pika research (brooksgroves.com)'})
            j=json.load(urllib.request.urlopen(req,timeout=60))
            res[f'n{r}']=j['total_results']
            res[f'obs{r}']=[dict(id=o['id'],date=o.get('observed_on'),q=o.get('quality_grade'),place=o.get('place_guess'),loc=o.get('location'),acc=o.get('positional_accuracy'),obscured=o.get('obscured')) for o in j['results']]
        except Exception as e:
            res[f'err{r}']=str(e)
        time.sleep(1.2)
    out.append(res); print(lab, res.get('n3'), res.get('n10'))
json.dump(out,open('probe/inat_sites.json','w'),indent=1)
