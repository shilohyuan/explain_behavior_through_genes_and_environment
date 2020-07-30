import os
import csv
import pickle
import folium
from circleMarkLayer import colorLayer, find
from TwinLayer import baseLayer, find
from collections import OrderedDict
os.chdir('/Users/Shiloh/Box/Shiloh_Geography_Spring2019')

# IDlatlon saved all points (not divided into individual) in a list[,]
# Michigan_ID, latitude, longitude, DateTime, sampleTimeZone, appType

with open('IDlatlon_all','rb') as f:
    IDlatlon = pickle.load(f)

with open('id_mapping.csv', 'rt'
                            '') as csvfile:
    id_mapping = csv.reader(csvfile, delimiter=',')
    for row in id_mapping:
        print(row)
        twinA_SVID += [row[0]]
        twinA_MichiganID += [row[2]]
        twinB_SVID += [row[1]]
        twinB_MichiganID += [row[3]]
        twinzygos += [row[4]]

for i in range(0,len(twinA_SVID)):
    print("At individual:",i+1)
    twinA = find(Michigan_ID, twinA_MichiganID[i])
    twinB = find(Michigan_ID, twinB_MichiganID[i])
    mapit = folium.Map(location=[IDlatlon[twinA[0]][1], IDlatlon[twinA[0]][2]], zoom_start=4)
    # create base map
    #(__mapit, __IDlatlon, __person, __subsetIndex, __twinIndex)
    mapitTwinA = baseLayer(mapit, IDlatlon, twinA, 1, 1, twinA_SVID[i]) #navy + white
    # plot twin A as twin reference with white square and number
    mapitTwinB = colorLayer(mapitTwinA, IDlatlon, twinB, 1, 2, twinB_SVID[i])#fuschia + default color
    # plot twin B with interactive layer and color
    folium.LayerControl().add_to(mapitTwinB)

    fileName = '{zygos}_{twinA}_{twinB}.html'.format(zygos = twinzygos[i], twinA = twinA_SVID[i], twinB=twinB_SVID[i])
    os.chdir('/Users/Shiloh/Box/Shiloh_Geography_Spring2019/foliumMap')# new directory
    mapitTwinB.save(fileName)

# for i in range(0,len(noPair)):
#     print('At individual:', i+1)
#     twin = find(Michigan_ID, noPair[i][0])
#     mapit = folium.Map(location = [IDlatlon[twin[0]][1], IDlatlon[twin[0]][2]], zoom_start =4 )
#     mapitTwinA = colorLayer(mapit, IDlatlon, twin, 1, 2, noPair[i][1])
#     folium.LayerControl().add_to(mapit)
#     fileName = '{twinSV}.html'.format(twinSV = noPair[i][1])
#     os.chdir('/Users/Shiloh/Box/Shiloh_Geography_Spring2019/foliumMap')
#     mapit.save(fileName)