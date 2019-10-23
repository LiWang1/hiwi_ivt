import pandas as pd
import geopandas as gpd
import timeit
import geopy.distance
import numpy as np
import timeit
def distance(p1, p2):
    return geopy.distance.vincenty(p1, p2).km

def closest(pt, others, real_dist):
    return min(others, key = lambda i: abs(distance(pt, i)-real_dist))

def closest_home(a, b, real_dist):
    e = [(Pa, Pb) for Pa in a for Pb in b]
    return(e[np.argmin([abs(distance(Pa, Pb)-real_dist) for (Pa, Pb) in e])])

def sample(minx, miny, maxx, maxy):
    samples = np.random.random(size = (200,2))
    samples[:,0] = miny+samples[:,0]*(maxy-miny)
    samples[:,1] = minx+samples[:,1]*(maxx-minx)
    return samples

def weird_division(n, d):
    return n / d if d else 0

# to get the x y boundaries for each trip
df_shapes = gpd.read_file("/Users/wangli/Desktop/coor/tl_2018_06_tract.shp")
df_shapes["GEOID"] = pd.to_numeric(df_shapes["GEOID"])

df_persons = pd.read_csv("/Users/wangli/Desktop/coor/cal_alldays_0815_withoutflight.csv",sep=",")
n_slice = len(df_persons)
indices = list(range(n_slice))

ori_minx = []
ori_maxx = []
ori_miny = []
ori_maxy = []

des_minx = []
des_maxx = []
des_miny = []
des_maxy = []

for i in indices:
    zone_shape_ori = df_shapes[df_shapes["GEOID"] == df_persons["origin_zone"][i]]["geometry"]
    zone_shape_des = df_shapes[df_shapes["GEOID"] == df_persons["destination_zone"][i]]["geometry"]
    if(len(zone_shape_ori)==1 and len(zone_shape_des)==1):
        bounds_ori = zone_shape_ori.bounds
        bounds_des = zone_shape_des.bounds
        ori_minx.append(bounds_ori["minx"])
        ori_maxx.append(bounds_ori["maxx"])
        ori_miny.append(bounds_ori["miny"])
        ori_maxy.append(bounds_ori["maxy"])
        des_minx.append(bounds_des["minx"])
        des_maxx.append(bounds_des["maxx"])
        des_miny.append(bounds_des["miny"])
        des_maxy.append(bounds_des["maxy"])

    else:
        ori_minx.append(0)
        ori_maxx.append(0)
        ori_miny.append(0)
        ori_maxy.append(0)
        des_minx.append(0)
        des_maxx.append(0)
        des_miny.append(0)
        des_maxy.append(0)

# coordinates generation
d = df_persons
coordinates_ori = []
coordinates_des = []
m_slice = len(d)
indices2 = list(range(m_slice))
for i in indices2:
    if(d['tripno'][i] == 1):
        ori_coor = sample(float(ori_minx[i]),float(ori_miny[i]),float(ori_maxx[i]),float(ori_maxy[i]))
        des_coor = sample(float(des_minx[i]),float(des_miny[i]),float(des_maxx[i]),float(des_maxy[i]))
        m=d['air_dist_km'][i]
        closer_coors = closest_home(ori_coor, des_coor, m)
        coordinates_ori.append(closer_coors[0])
        coordinates_des.append(closer_coors[1])
    elif(d['tripno'][i] > 1 and d['destination_purpose'][i]!='Home'):
        ori_coor = coordinates_des[i-1]
        des_coor = sample(float(des_minx[i]),float(des_miny[i]),float(des_maxx[i]),float(des_maxy[i]))
        des_coor_result = closest(ori_coor, des_coor, d['air_dist_km'][i])
        coordinates_ori.append(ori_coor)
        coordinates_des.append(des_coor_result)
    elif(d['tripno'][i] > 1 and d['destination_purpose'][i]=='Home'):
        ori_coor = coordinates_des[i-1]
        diff = int(d['tripno'][i]-1)
        des_coor = coordinates_ori[i-diff]
        coordinates_ori.append(ori_coor)
        coordinates_des.append(des_coor)

# save 
d["original_coordinates"] = coordinates_ori
d["destination_coordinates"] = coordinates_des
d.to_csv('cal_withoutflights_coord0818.csv')
