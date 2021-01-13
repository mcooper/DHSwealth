import ee
import pandas as pd

sel = pd.read_csv('~/mortalityblob/dhs/wealthvars_geo.csv')

ghsl = ee.Image("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1").select('built')

sel = sel[~sel['latitude'].isna()]

points = []
ints = []
for row in sel.iterrows():
    if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
        geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(15000)
        feat = ee.Feature(geom, {'code':row[1]['code']})
        points.append(feat)

features = []
i = 0
while i < len(points):
    j = i + 3000
    fc = ee.FeatureCollection(points[i:j])
    features.append(fc)
    i = j

accum = pd.DataFrame()
for f in features:
    print(features.index(f))
    res = ghsl.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=f).getInfo()
    for i in res['features']:
        hist = i['properties']['histogram']
        hist['code'] = i['properties']['code']
        accum = accum.append(pd.DataFrame(hist, index=[0]))

accum = accum.fillna(0)

accum.to_csv('~/mortalityblob/dhs/wealthvars_built_raw.csv', index=False)
