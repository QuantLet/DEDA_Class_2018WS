# -*- coding: utf-8 -*-

"""
Created Jan 11, 2018
@author: Alex Truesdale

Python module for performing spatial clustering.
"""

import hdbscan
import pandas as pd
import numpy as np
from shapely.geometry import MultiPoint

def clusterer(objects_in, collection_in, min_cluster_size, min_samples, first_run):
    """Application of the HDBSCAN algorithm over n parameter groupings."""

    # Run algorithm with haversine distance and ball-tree algorithm.
    clusterer = hdbscan.HDBSCAN(metric = 'haversine',
                                min_cluster_size = min_cluster_size,
                                min_samples = min_samples)

    hdb = clusterer.fit(np.radians(objects_in))

    # Count clusters and organise as pd.Series.
    cluster_labels = hdb.labels_
    count_clusters = len(set(cluster_labels))
    print('------ {} clusters identified \n'.format(count_clusters))
    clusters = pd.Series([objects_in[cluster_labels == n] for n in range(count_clusters - 1)])

    # Store cluster locations in external list for proofing against original data.
    clustered_locations = []
    for cluster in clusters:
        for item in cluster:
            if first_run:
                lat = round(item[0], 6)
                lng = round(item[1], 6)
                location = collection_in.loc[(collection_in['lat'] == lat) & (collection_in['lng'] == lng)]
                clustered_locations.append([location['lat'], location['lng']])
            else:
                clustered_locations.append([item[0], item[1]])

    return clusters, clustered_locations

def get_centermost_point(cluster):
    """Centroid (within cluster geometry) finder using MultiPoint library."""

    multipoint = MultiPoint(cluster)
    point = (multipoint.centroid.x, multipoint.centroid.y)
    return point
