"""
American pika: do the pre-1950 museum localities still have pikas near them?

Reads gbif/data/ochotona-princeps.csv.gz (GBIF download doi:10.15468/dl.p7sdx7, with Copernicus GLO-30
ground elevation added by gbif/fetch.py). Historical localities = pre-1950 records with coordinate
uncertainty <= 2 km, grouped into ~1 km cells, using the collector's label elevation where there is one.
A locality counts as re-found if a 2000+ record (uncertainty <= 1 km or unstated) lies within 3 km.

    python gbif/pika/analyze.py     # numbers -> out/
    python gbif/pika/figs.py        # map.png, latitude.png, refound.png -> out/
Needs pandas, numpy, scikit-learn (+ geopandas, matplotlib and Natural Earth shapefiles in NE for figs).
"""
import pandas as pd, numpy as np, json
from sklearn.neighbors import BallTree
from pathlib import Path
HERE=Path(__file__).resolve().parent
DATA=HERE.parent/'data'/'ochotona-princeps.csv.gz'
OUT=HERE/'out'; OUT.mkdir(exist_ok=True)
R=6371.0
d=pd.read_csv(str(DATA), low_memory=False)
d=d[d.lon.between(-125.5,-103)&d.lat.between(32,58)&d.year.notna()&d.dem_m.notna()].copy()
d['era']=pd.cut(d.year,[0,1949,1999,2100],labels=['pre1950','1950-99','2000+'])
# latitude-adjusted residual: elevation minus median of ALL records in its 1-degree band
d['band']=np.floor(d.lat)
d['resid']=d.dem_m-d.groupby('band').dem_m.transform('median')
hist=d[(d.era=='pre1950')&(d.uncert_m<=2000)].copy()
# trust the collector's label elevation where there is one (e.g. 1919 Rainier specimens georeferenced to the summit)
bandmed=d.groupby('band').dem_m.median()
hist['dem_m']=hist.elev_given_m.where(hist.elev_given_m.notna(),hist.dem_m)
hist['resid']=hist.dem_m-hist.band.map(bandmed)
print('label-elev used',hist.elev_given_m.notna().sum(),'of',len(hist))
mod=d[(d.era=='2000+')&((d.uncert_m<=1000)|d.uncert_m.isna())].copy()
# historical localities: unique ~1 km cells
hist['cell']=list(zip((hist.lat/0.01).round(),(hist.lon/0.01).round()))
loc=hist.groupby('cell').agg(lat=('lat','mean'),lon=('lon','mean'),elev=('dem_m','median'),resid=('resid','median'),
     year=('year','min'),n=('year','size'),state=('state','first')).reset_index(drop=True)
tree=BallTree(np.radians(mod[['lat','lon']].values),metric='haversine')
out={}
for km in (1,3,5):
    idx=tree.query_radius(np.radians(loc[['lat','lon']].values),r=km/R)
    loc[f'found{km}']=[len(i)>0 for i in idx]
    loc[f'dmed{km}']=[np.median(mod.dem_m.values[i])-e if len(i) else np.nan for i,e in zip(idx,loc.elev)]
    loc[f'dmin{km}']=[np.min(mod.dem_m.values[i])-e if len(i) else np.nan for i,e in zip(idx,loc.elev)]
    out[km]=dict(found=int(loc[f'found{km}'].sum()),of=len(loc),
                 dmed_median=float(np.nanmedian(loc[f'dmed{km}'])),dmin_median=float(np.nanmedian(loc[f'dmin{km}'])),
                 share_mod_higher=float((loc[f'dmed{km}']>0).sum()/loc[f'found{km}'].sum()))
print(json.dumps(out,indent=1))
loc['third']=pd.qcut(loc.resid,3,labels=['low','middle','high'])
print(loc.groupby('third',observed=True).agg(n=('lat','size'),found3=('found3','mean'),elev=('elev','median'),resid=('resid','median'),dmed3=('dmed3','median')))
# found by elevation-relative (low third) and state
loc['st']=loc.state.str.replace(r' \(.*','',regex=True).str.title()
print(loc.groupby('st').agg(n=('lat','size'),found3=('found3','mean'),dmed3=('dmed3','median')).query('n>=10').sort_values('found3'))
loc.to_csv(str(OUT)+'/hist_localities.csv',index=False)
# lower edge by era: 10th percentile per band, bands with >=20 in both
q=d[d.era.isin(['pre1950','2000+'])].groupby(['band','era'],observed=True).dem_m.agg(['size',lambda s:s.quantile(.1),'median'])
q.columns=['n','p10','med']; q=q.unstack()
q=q[(q['n']>=20).all(axis=1)]
print(q)
print('p10 change median', (q['p10']['2000+']-q['p10']['pre1950']).median(), 'median change', (q['med']['2000+']-q['med']['pre1950']).median())
