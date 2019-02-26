# -*- coding: utf-8 -*-

"""
Created Jan 27, 2018
@author: Alex Truesdale

Jupyter Notebook for running regression on data.
"""

import os
import time
import numpy as np
import pandas as pd
import statsmodels.api as sm

# Introduction.
print('==============================================================================')
print("\n This script reads in the final data collected for this project. The data \n \
selection here contains a number of features relating to distances to \n \
certain amenities, area activity, ratings of nearby locations, and more. \n \
This data is regressed (OLS) against listing prices for ~5000+ apartment \n \
listings to study the effects of these social components. \n")
print('==============================================================================')
print()
time.sleep(8)

# Read in data.
print("------ reading data from 'final_listings.csv'")
listings = pd.read_csv('../final_listings.csv')
X, y = listings.iloc[:, :-3], np.log(listings.iloc[:, -1]).values.reshape((-1, 1))
time.sleep(1)

# Fit regression model.
print("------ defining model and fitting data \n")
regression_model = sm.OLS(y, X)
regression_fit = regression_model.fit()
time.sleep(2.5)

# Summary of model.
summary = regression_fit.summary()
print(summary)
time.sleep(1.5)

# Output summary at HTML and read back in as DataFrame.
print()
print("------ extracting R sq. and P values to 'regression_summary.csv' \n")
results_as_html = summary.tables[1].as_html()
summary_df = pd.read_html(results_as_html, header = 0, index_col = 0)[0]
summary_df = summary_df.loc[:, ['coef', 'P>|t|']]
summary_df['P>|t|'] = round(summary_df['P>|t|'], 3)
summary_df['coef'] = round((np.exp(summary_df['coef']) - 1) * 100, 4)
summary_df.columns = ['Unit Coef %', 'P>|t|']
summary_df.to_csv('regression_summary.csv')
time.sleep(1.5)
