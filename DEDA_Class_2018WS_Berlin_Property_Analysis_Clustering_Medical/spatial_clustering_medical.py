# -*- coding: utf-8 -*-

"""
Created Jan 11, 2018
@author: Alex Truesdale

Jupyter Notebook for performing spatial clustering on hospital / medical location data.
"""

import os
import time
import gmplot
import pandas as pd
import clustering_module as clusterer

# Introduction.
print('==================================================================================')
print("\n This script reads in medical centre spatial data collected for this project. \n \
The data selection here contains the lat, lng coordinates of all medical \n \
centres in the city. These points are then clustered using the HDBSCAN \n \
algorithm, plotted on a Google Maps instance, and written to an HTML file, \n \
which can be opened in your browser. \n")
print('==================================================================================')
print()
time.sleep(7)

# Search for all university data; return as list of objects; create coordinate matrix of points.
print("------ reading data from 'medical.csv' as lat, lng values")
medical_df = pd.read_csv('medical.csv')
medical_coords = medical_df[['lat', 'lng']].values
time.sleep(1)

# First pass of clustering (greedy).
print("------ count of medical locations: {}".format(len(medical_coords)))
time.sleep(2)

print("------ running clustering module")
clusters, clustered_locations = clusterer.clusterer(medical_coords,
                                                    medical_df,
                                                    min_cluster_size = 5,
                                                    min_samples = 5,
                                                    first_run = True)

# Find centremost points of clusters; reorder into new matrix.
print("------ find centremost points of clusters")
time.sleep(1)
centremost_points = clusters.map(clusterer.get_centermost_point)
centremost_df = pd.DataFrame(list(centremost_points), columns = ['lat', 'lng'])
centremost_coords = centremost_df[['lat', 'lng']].values
time.sleep(1)

# Identify non-clustered clusters.
dict_centroids = {i: list(centroid) for i, centroid in enumerate(centremost_points)}

# Define lat long coordinate lists.
print("------ collect new and original lat, lng points")
hospital_lat_list = tuple([hospital[0] for hospital in medical_coords])
hospital_lng_list = tuple([hospital[1] for hospital in medical_coords])

centre_lat_list = tuple([point[0] for point in centremost_points])
centre_lng_list = tuple([point[1] for point in centremost_points])
time.sleep(1)

# Change dir. to visualisation dir.
print("------ changing directory to 'runtime_generated' \n")
os.chdir('runtime_generated')
time.sleep(2)

# Initialise map.
print("------ initialising blank map for plotting")
print("------ NOTE: this map will have a 'developer only' banner over it. For \n \
            the maps seen in 'pre_generated/output_maps', one must have \n \
            a valid Google Cloud project with an API key that is  \n \
            provisioned for using the JavaScript Maps API. \n")

time.sleep(6)

raw_map_out = gmplot.GoogleMapPlotter(52.5112264, 13.415641, 10.81)

# Plot scatter points.
print("------ plotting points: original and medical cluster centroids")
raw_map_out.scatter(hospital_lat_list, hospital_lng_list, '#07B3BC', size = 40, marker = False)
raw_map_out.scatter(centre_lat_list, centre_lng_list, '#F76A7D', size = 130, marker = False)
time.sleep(1.5)

# Trigger write / compile method to create final map.
raw_map_out.draw("medical.html")
print("------ saving map as: 'medical.html' in 'runtime_generated' \n")
time.sleep(.5)
