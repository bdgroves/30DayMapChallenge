"""Figures for the pika post (see analyze.py). Run analyze.py first."""
import pandas as pd, numpy as np, geopandas as gpd
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
from shapely.geometry import box
import os
from pathlib import Path
HERE=Path(__file__).resolve().parent
OUT=HERE/'out'
NE=os.environ.get('NE','natural-earth-vector')   # a clone of nvkelso/natural-earth-vector
FONTS=os.environ.get('FONTS','')
for n in ("Roboto-Regular.ttf","Roboto-Medium.ttf"):
    font_manager.fontManager.addfont(f"{FONTS}/{n}") if FONTS else None
PAPER, INK, STONE, MIST, CREAM = "#f5f0e8", "#1c1a16", "#7a7268", "#d6d0c4", "#ede8dc"
FOUND, LOST, NEUTRAL = "#2e7aa6", "#c8602e", "#b8b0a4"
plt.rcParams.update({"font.family":"Roboto","font.size":10,"axes.edgecolor":MIST,"axes.labelcolor":STONE,
  "xtick.color":STONE,"ytick.color":STONE,"axes.facecolor":PAPER,"figure.facecolor":PAPER,"savefig.facecolor":PAPER,
  "axes.spines.top":False,"axes.spines.right":False})
d=pd.read_csv(HERE.parent/'data'/'ochotona-princeps.csv.gz', low_memory=False)
d=d[d.lon.between(-125.5,-103)&d.lat.between(32,58)&d.year.notna()&d.dem_m.notna()]
mod=d[d.year>=2000]
loc=pd.read_csv(str(OUT)+'/hist_localities.csv')
CRS="+proj=aea +lat_1=38 +lat_2=52 +lat_0=44 +lon_0=-115 +datum=WGS84 +units=m"
clip=box(-131,30,-99,60)
land=gpd.read_file(f'{NE}/10m_physical/ne_10m_land.shp').clip(clip).to_crs(CRS)
lines=gpd.read_file(f'{NE}/50m_cultural/ne_50m_admin_1_states_provinces_lines.shp').clip(clip).to_crs(CRS)
ctry=gpd.read_file(f'{NE}/50m_cultural/ne_50m_admin_0_countries.shp').clip(clip).to_crs(CRS)
lakes=gpd.read_file(f'{NE}/50m_physical/ne_50m_lakes.shp').clip(clip).to_crs(CRS)
def pts(df): return gpd.GeoSeries(gpd.points_from_xy(df.lon,df.lat),crs=4326).to_crs(CRS)

# ── 1. map ──
fig,ax=plt.subplots(figsize=(7.6,9.4))
land.plot(ax=ax,color=CREAM,ec="#c9c1b3",lw=0.5,zorder=0)
lakes.plot(ax=ax,color=PAPER,ec="none",zorder=1)
lines.plot(ax=ax,color=MIST,lw=0.6,zorder=1)
ctry.boundary.plot(ax=ax,color="#b9b1a3",lw=0.9,zorder=1)
m=pts(mod); ax.scatter(m.x,m.y,s=1.6,color=NEUTRAL,lw=0,alpha=0.55,zorder=2,rasterized=True)
f=loc[loc.found3]; l=loc[~loc.found3]
pf,pl=pts(f),pts(l)
ax.scatter(pf.x,pf.y,s=26,color=FOUND,ec=PAPER,lw=0.8,zorder=4,label=f"Pre-1950 site with a pika record since 2000 within 3 km ({len(f)})")
ax.scatter(pl.x,pl.y,s=26,color=LOST,ec=PAPER,lw=0.8,zorder=3,label=f"Pre-1950 site with no record since 2000 within 3 km ({len(l)})")
ax.scatter([],[],s=8,color=NEUTRAL,label="Pika record since 2000 (mostly iNaturalist)")
x0,y0=pts(pd.DataFrame({'lon':[-125.6],'lat':[33.6]})).iloc[0].coords[0]
x1,y1=pts(pd.DataFrame({'lon':[-103.6],'lat':[56.4]})).iloc[0].coords[0]
ax.set_xlim(-1.30e6,1.05e6); ax.set_ylim(-1.25e6,1.38e6)
lab=[("Sierra\nNevada",-119.6,37.2,"right"),("Great Basin",-116.8,39.6,"center"),("Southern\nRockies",-106.3,38.6,"center"),
     ("Columbia River\nGorge",-122.6,45.25,"right"),("Cascades",-121.0,48.2,"right"),("Canadian\nRockies",-118.6,52.6,"right"),
     ("Mount Rainier ▸",-122.0,46.85,"right")]
for t,lo,la,ha in lab:
    p=pts(pd.DataFrame({'lon':[lo],'lat':[la]})).iloc[0]
    ax.text(p.x,p.y,t,fontsize=8.2,color=INK,ha=ha,va="center",style="italic",zorder=6,
            bbox=dict(boxstyle="round,pad=0.15",fc=CREAM,ec="none",alpha=0.75))
ax.set_aspect("equal"); ax.axis("off")
ax.legend(loc="lower left",frameon=True,facecolor=PAPER,edgecolor="none",framealpha=0.9,fontsize=8,labelcolor=INK,
          markerscale=1.0,borderpad=0.6,bbox_to_anchor=(0.0,0.0))
ax.text(0,1.045,"Where the old pika sites are, and who's been back",transform=ax.transAxes,fontsize=14,color=INK,fontweight="medium")
ax.text(0,1.015,f"{len(loc)} museum-specimen localities from 1871–1949, checked against {len(mod):,} records since 2000",
        transform=ax.transAxes,fontsize=9,color=STONE)
ax.text(1,-0.01,"Data: GBIF.org occurrence download doi:10.15468/dl.p7sdx7 · Natural Earth · Brooks Groves",
        transform=ax.transAxes,fontsize=7,color=STONE,ha="right",va="top")
fig.savefig(str(OUT)+'/map.png',dpi=200,bbox_inches="tight"); plt.close(fig)

# ── 2. elevation vs latitude ──
fig,ax=plt.subplots(figsize=(9,5.4))
ax.scatter(mod.lat,mod.dem_m,s=2.5,color=NEUTRAL,alpha=0.45,lw=0,rasterized=True,label="Pika record since 2000")
ax.scatter(f.lat,f.elev,s=22,color=FOUND,ec=PAPER,lw=0.7,zorder=4,label="Pre-1950 site, re-found within 3 km")
ax.scatter(l.lat,l.elev,s=22,color=LOST,ec=PAPER,lw=0.7,zorder=3,label="Pre-1950 site, no record since 2000 within 3 km")
ax.set_xlim(33.3,56.5); ax.set_ylim(-80,4500)
ax.set_xlabel("Latitude (°N)"); ax.set_ylabel("Ground elevation at the record (m)")
ax.grid(axis="y",color=MIST,lw=0.6); ax.set_axisbelow(True)
notes=[(36.2,1250,"Sierra Nevada:\nmostly above 2,500 m"),(45.6,300,"Columbia River Gorge:\npikas near sea level"),
       (53.6,3150,"BC & Alberta:\nthe floor keeps dropping")]
for x,y,t in notes: ax.text(x,y,t,fontsize=8.3,color=INK,ha="center",va="center")
ax.annotate("",xy=(45.62,90),xytext=(45.6,200),arrowprops=dict(arrowstyle="-",color=STONE,lw=0.7))
ax.legend(loc="upper right",frameon=False,fontsize=8.3,labelcolor=INK,markerscale=1.2,bbox_to_anchor=(1,1.0))
ax.annotate("",xy=(36.6,2350),xytext=(36.3,1450),arrowprops=dict(arrowstyle="-",color=STONE,lw=0.7))
ax.text(0,1.07,"The farther north, the lower the pika lives",transform=ax.transAxes,fontsize=13.5,color=INK,fontweight="medium")
ax.text(0,1.025,"Each dot is a GBIF record, placed at the ground elevation of its coordinates (Copernicus GLO-30)",
        transform=ax.transAxes,fontsize=9,color=STONE)
fig.savefig(str(OUT)+'/latitude.png',dpi=200,bbox_inches="tight"); plt.close(fig)

# ── 3. re-found rate by relative elevation ──
from sklearn.neighbors import BallTree
t=BallTree(np.radians(mod[['lat','lon']].values),metric='haversine')
loc['near15']=[len(i) for i in t.query_radius(np.radians(loc[['lat','lon']].values),r=15/6371)]
loc['third']=pd.qcut(loc.resid,3,labels=['Lowest third','Middle third','Highest third'])
a=loc.groupby('third',observed=True).found3.agg(['mean','size'])
b=loc[loc.near15>=5].groupby('third',observed=True).found3.agg(['mean','size'])
print(a,b)
fig,ax=plt.subplots(figsize=(8.4,3.9))
yy=np.arange(3)[::-1]; h=0.36
ax.barh(yy+h/2,a['mean']*100,h,color=FOUND,label="All pre-1950 sites")
ax.barh(yy-h/2,b['mean']*100,h,color="#93bcd6",label="Only sites with 5+ modern pika records within 15 km (someone is looking nearby)")
for y,v,n in zip(yy+h/2,a['mean'],a['size']): ax.text(v*100+1,y,f"{v*100:.0f}%  ({n} sites)",va="center",fontsize=8.5,color=INK)
for y,v,n in zip(yy-h/2,b['mean'],b['size']): ax.text(v*100+1,y,f"{v*100:.0f}%  ({n})",va="center",fontsize=8.5,color=INK)
ax.set_yticks(yy,["Lowest third\nfor their latitude","Middle third","Highest third"])
ax.set_xlim(0,100); ax.set_xticks([0,25,50,75,100],["0%","25%","50%","75%","100%"])
ax.tick_params(axis='y',length=0); ax.spines['left'].set_visible(False)
ax.grid(axis="x",color=MIST,lw=0.6); ax.set_axisbelow(True)
ax.set_xlabel("Share of old sites with a pika record since 2000 within 3 km")
ax.legend(loc="upper left",frameon=False,fontsize=8.3,labelcolor=INK,bbox_to_anchor=(-0.02,-0.2),ncol=2)
ax.text(0,1.13,"The low sites are the ones nobody's re-finding",transform=ax.transAxes,fontsize=13.5,color=INK,fontweight="medium")
ax.text(0,1.05,"Pre-1950 pika localities split into thirds by how low they sit compared with other records at the same latitude",
        transform=ax.transAxes,fontsize=9,color=STONE)
fig.savefig(str(OUT)+'/refound.png',dpi=200,bbox_inches="tight"); plt.close(fig)
