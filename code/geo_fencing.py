"""
# 金融与经济大数据挖掘期末作业
# 沈嘉伦 16307110030
------
对station.csv进行处理，添加每个station对应的行政区
Reference:
https://www.cnblogs.com/huang-yc/p/10122112.html
"""
import numpy as np
import pandas as pd
#import matplotlib as mpl
from shapely.geometry import Point
from shapely.geometry.polygon import Polygon


#mobile_data_path = '../data/mobile.csv'
station_data_path = '../data/station.csv'
map_data_path = '../data/sh_map_data.csv'

#mobile_data = pd.read_table(mobile_data_path, sep='\t')
station_data = pd.read_table(station_data_path, sep='\t')
map_data = pd.read_table(map_data_path, sep=',')
map_data = map_data[['NAME','long','lat']]

#mobile_data.info()
station_data.info()
map_data.info()
station_data.head()
map_data.head()

#原始数据中没有缺失值
#mobile_data.isnull().sum()
#station_data.isnull().sum()

districts = list(set(map_data['NAME'])) #所有区名
distPolys = dict()

# 获得所有区的多边形
for district in districts:
    lst = []
    df = map_data[map_data['NAME'] == district]
    for i, row in df.iterrows():
        lng = row['long'] #x
        lat = row['lat'] #y
        lst.append((lng, lat))
    distPolys[district] = Polygon(lst)

def in_district(position, district):
    """
    判断一个点是否在多边形内
    In: 
        position: a list [x,y] ([long,lat])
        district: a string, the district name
    Out:
        True if position is in district; False otherwise
    """
    assert district in districts, "%s not in districts" % district
    poly = distPolys[district]
    point = Point(position)
    return poly.contains(point)

def in_which_district(position):
    """
    判断位置属于哪个区
    In: 
        position: a list [x,y] ([long,lat])
    Out:
        district: a string, the district name the position is in
    """
    for district in districts:
        if in_district(position, district):
            return district
    return 'Unknown'

stationDistLst = []
for i, row in station_data.iterrows():
    lng = row['lng']
    lat = row['lat']
    dis = in_which_district((lng, lat))
    stationDistLst.append(dis)

station_data['district'] = stationDistLst

#保存
outpath = '../data/station_data_district.csv'
station_data.to_csv(outpath, index=False)