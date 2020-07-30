# Do not run .py again because it would change the order in Michigan_ID_
# IDlatlon saved all points (not divided into individual) in a list
import csv
import os
import pickle

os.chdir('/Users/Shiloh/Box/Shiloh_Geography_Spring2019')

IDlatlon = []
Michigan_ID = []

with open('/Users/Shiloh/Box/Shiloh_Geography_Spring2019/location_row_noNA.txt', 'r') as f:
    reader = csv.reader(f, delimiter = '\t')
    for row in reader:
        bMichigan_ID += [row[1]]
        IDlatlon += [[row[1],row[2],row[3],row[0],row[6],row[7]]]
                    #Michigan_ID, latitude, longitude, DateTime, sampleTimeZone,appType
del IDlatlon[0]
del Michigan_ID[0]

Michigan_IDUnique = set(Michigan_ID)
Michigan_IDNew = list(Michigan_IDUnique)

with open('Michigan_ID_mapping','wb') as f:
    pickle.dump(Michigan_IDNew,f)
with open('IDlatlon_all','wb') as f:
    pickle.dump(Michigan_IDNew,f)

