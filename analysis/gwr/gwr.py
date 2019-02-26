# -*- coding: utf-8 -*-

"""
Created Feb 25, 2019
@author: Alex Truesdale

Jupyter Notebook for running regression on data.
"""

import os
import csv
import sys
import time
import numpy as np
import pandas as pd

from io import StringIO
from mgwr.gwr import GWR, MGWR
from mgwr.sel_bw import Sel_BW

df = pd.DataFrame

# Introduction.
print('===========================================================================')
print("\n This script reads in the final data collected for this project. The data \n \
selection here contains a number of features relating to distances to \n \
certain amenities, area activity, ratings of nearby locations, and more. \n \
This data is regressed (GWR) against listing prices for ~5000+ apartment \n \
listings to study the effects of these social components. \n")
print('===========================================================================')
print()
time.sleep(8)

# Read in data.
print("------ reading data from 'final_listings.csv'")
listings = pd.read_csv('../final_listings.csv')
time.sleep(1)

# Feature subset definition.
# listings = listings[['balcony', 'built_in_kitchen', 'cellar', 'courtage', 'garden',
#                      'is_vacant', 'lift', 'living_space', 'number_rooms', 'private_seller',
#                      'lat', 'lng', 'price']]

# Define predictors and target values.
print("------ defining target and covariates")
X, y = listings.iloc[:, :-3].values, np.log(listings.iloc[:, -1]).values.reshape((-1, 1))
X_columns = ['intercept'] + list(listings.iloc[:, :-3].columns)
lat = listings['lat']
lng = listings['lng']
coordinates = list(zip(lng, lat))
time.sleep(1)

# GWR
print("------ performing model bandwidth search...")
gwr_selector = Sel_BW(coordinates, y, X, spherical = True)
gwr_bandwidth = gwr_selector.search(criterion = 'AICc')
print("------ bandwidth identified: {} \n".format(gwr_bandwidth))
time.sleep(1)

print("------ fitting GWR model \n")
gwr_results = GWR(coordinates, y, X, gwr_bandwidth, spherical = True).fit()
print(gwr_results.summary())
time.sleep(12)

class Capturing(list):
    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self

    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        del self._stringio
        sys.stdout = self._stdout

with Capturing() as output:
     gwr_results.summary()

print()
print("------ stripping output from model summary")
# Feature subset cuts.
# gwr_stats_meta = output[31:33] + output[36:47]
# gwr_stats_features = [output[50]] + output[52:63]

#Full features cuts.
gwr_stats_meta = output[58:60] + output[63:74]
gwr_stats_features = [output[77]] + output[79:117]
time.sleep(1)

gwr_stats_meta = [[line.split(':')[0].strip(), line.split(':')[1].strip()] for line in gwr_stats_meta]
gwr_stats_meta = df(gwr_stats_meta, columns = ['key', 'value'])
gwr_stats_meta.to_csv('gwr_stats_meta.csv', header = None, index = None)
print("------ model meta data: \n")
time.sleep(4)

print(gwr_stats_meta)
time.sleep(4)

gwr_stats_features = [[line.split()[0].strip(), line.split()[1].strip(), line.split()[2].strip(), line.split()[3].strip(), line.split()[4].strip(), line.split()[5].strip()] for line in gwr_stats_features]
gwr_stats_features = df(gwr_stats_features[1:], columns = gwr_stats_features[0])
gwr_stats_features['Variable'] = X_columns
gwr_stats_features['Unit Coef %'] = (np.exp(pd.to_numeric(gwr_stats_features['Mean'])) -1) * 100
gwr_stats_features['Unit Coef %'] = gwr_stats_features['Unit Coef %'].round(decimals = 4)
gwr_stats_features.to_csv('gwr_stats_features.csv', header = True, index = None)
print()
print("------ model feature effects: \n")
time.sleep(4)

print(gwr_stats_features)
time.sleep(6)

print()
print("------ saving dataframes to: 'gwr_stats_meta.csv' & 'gwr_stats_features.csv' \n")
time.sleep(1.5)
