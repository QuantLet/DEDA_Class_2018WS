#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Mar  9 18:23:12 2019

@author: annasemenova
"""

import pandas as pd
pd.options.display.max_columns = None
import warnings
warnings.simplefilter('ignore')
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import accuracy_score, balanced_accuracy_score, f1_score
from sklearn.pipeline import Pipeline
from sklearn.model_selection import train_test_split, GridSearchCV
import matplotlib.pyplot as plt
import seaborn as sns
from imblearn.ensemble import BalancedRandomForestClassifier

print(''' 
This file presents working code. 
Example of data is submitted to show that the code is working. Real data is not available for confidentiality reasons. 
Results achieved on real data can be found in jupyter notebook attached. 

''')


data = pd.read_csv('data.csv')

regions = [
        'europe', 'eu', 'dach', 'nordics','western_europe',
        'eastern_europe', 'southern_europe','north_america', 'asia', 'brics',
        'baltics', 'cw_of_ind_states', 'lat_america','near_east', 'north_africa',
        'ssh_africa', 'oceania'
    ]


def preprocess(data):
    # Create time passed since a funding round in years as diff between today and announce date
    data.announced_on = pd.to_datetime(data.announced_on)
    data['funding_ago'] = (pd.Timestamp.today() - data.announced_on).dt.days/365.25
    
    # Where age is missing add the time from the first funding round (if known)
    data['first_known_ago'] = data.groupby(['company_name_x', 'domain']).funding_ago.transform('max')
    data.age.fillna(data.first_known_ago, inplace=True)
    data.drop('first_known_ago', axis=1, inplace=True)
    
    
    # Encode investment types manually to preserve ordinal info
    invest_dict = {
        'angel':1, 'seed':2, 'series_a':3, 'series_b':4, 'series_c':4, 'series_d':4,
        'series_e':4, 'series_f':4, 'series_g':4, 'series_h':4, 'series_i':4
    }
    data.investment_type = data.investment_type.replace(invest_dict)
    # Encode all other investment types (unknown) as zero
    data.investment_type = pd.to_numeric(data.investment_type, errors='coerce').fillna(0)
    
    
    data.sort_values(['company_name_x', 'domain', 'announced_on'], ascending=False, inplace=True)

    
    # from the existing dataset leave only the last funding round (known or unknown) for each company
    data = data.drop_duplicates(['company_name_x', 'domain']).reset_index(drop=True)
    
    
    data.drop(['announced_on'], axis=1, inplace=True)
    
    
    # Deal with missing data - make a separate category 
    data.funding_ago.fillna(-1, inplace=True)
    data.age.fillna(-1, inplace=True)
    data.raised_amount_usd.fillna(-1, inplace=True)
    data.funding_total_usd.fillna(-1, inplace=True)
    data[regions] = data[regions].fillna(-1)
    
    # Leave only companies younger than 10 years
    data = data[(data.age < 10) & (data.age >= -1)]
    
    # Convert categorical data to numerical
    le = LabelEncoder()
    col_to_enc = ['Client_Focus__c', 'Main_Client_Sector__c', 'Business_Model__c']
    data[col_to_enc] = data[col_to_enc].fillna(0)
    data = data.apply(lambda x:
                      le.fit_transform(x.astype('str')) if x.name in col_to_enc else x
                     )   
    data.employee_count = le.fit_transform(data.employee_count.astype('int'))
    data = pd.concat([
        data, 
        data.Additional_Client_Sector__c.str.replace(' ', '').str.get_dummies(sep=';')
    ], axis=1)
    data.drop('Additional_Client_Sector__c', axis=1, inplace=True)
    data.drop('category_list', axis=1, inplace=True)
    return data


def get_train_data(data):
    
    data = data[data.investment_type > 0] 
    data.reset_index(drop=True, inplace=True)    
    
    plt.figure(figsize=(8, 5))
    ax = sns.barplot(x=data.investment_type.map({
        1:'Seed', 
        2:'Series_A', 
        3:'Series_B', 
        4:'Growth'
    }).value_counts(dropna=False).index,
                     y=data.investment_type.value_counts(dropna=False),
                     palette='BuPu')
    plt.ylabel('Value counts', fontsize=12)
    plt.xlabel('Deal stage', fontsize=12)
    fig1 = plt.gcf()
    plt.show()
    
    y = (data.investment_type - 1).values
    X = data.drop(['investment_type', 'company_name_x', 'domain'], axis=1)

    return X, y


def balancedrf_model(X_train, y_train):

    params={'classifier__n_estimators':[200],
            'classifier__criterion':['gini','entropy'],
            'classifier__min_samples_leaf':[3,5,7]}

    pipe = Pipeline([
                ('classifier', BalancedRandomForestClassifier())
                ])

    clf = GridSearchCV(pipe, params, cv = 3, scoring='f1_weighted')
    clf.fit(X_train,y_train)

    print('f1_weighted =', clf.best_score_)
    print('Best params =', clf.best_params_)
    
    return clf.best_estimator_


df = preprocess(data)
X, y = get_train_data(df)
X_train, X_test, y_train, y_test = train_test_split(X, 
                                                    y, 
                                                    test_size=.33, 
                                                    stratify=y,
                                                    random_state=241)
clf = balancedrf_model(X_train, y_train)
y_pred = clf.predict(X_test)
    
print('Accuracy =', accuracy_score(y_test, y_pred))
print('Balanced accuracy =', balanced_accuracy_score(y_test, y_pred))
print('F1 score =', f1_score(y_test, y_pred, average='weighted'))
    
mm_input = pd.DataFrame()
mm_input['y'] = y_test
mm_input['y_pred'] = y_pred

edges = mm_input.groupby('y')['y_pred'].value_counts()
m = pd.DataFrame(edges.unstack(level=-1))
m = m.div(m.sum(axis=1), axis=0)
a4_dims = (5, 5)
fig, ax = plt.subplots(figsize=a4_dims)
plt.title('Mismatches (ALL)')
sns.heatmap(m, annot=True, cmap='BuPu')
plt.show()

# binary classification model

def get_train_data(data):
    
    data = data[data.investment_type > 0]
    data.investment_type.replace(1, 0, inplace=True)
    data.investment_type.replace(2, 0, inplace=True)
    data.investment_type.replace(3, 1, inplace=True)
    data.investment_type.replace(4, 1, inplace=True)
    data.reset_index(drop=True, inplace=True)
    
    import seaborn as sns
    import matplotlib.pyplot as plt
    
    plt.figure(figsize=(8, 5))
    ax = sns.barplot(x=data.investment_type.map({
        0:'Early stage', 
        1:'Late stage'
    }).value_counts(dropna=False).index,
                     y=data.investment_type.value_counts(dropna=False),
                     palette='BuPu')
    plt.ylabel('Value counts', fontsize=12)
    plt.xlabel('Deal stage', fontsize=12)
    fig1 = plt.gcf()
    plt.show()
    
    y = data.investment_type.values
    X = data.drop(['investment_type', 'company_name_x', 'domain'], axis=1)

    return X, y

def bin_balancedrf_model(X_train, y_train):

    params={'classifier__n_estimators':[200],
            'classifier__criterion':['gini','entropy'],
            'classifier__min_samples_leaf':[3,5,7]}

    pipe = Pipeline([
                ('classifier', BalancedRandomForestClassifier())
                ])

    clf = GridSearchCV(pipe, params, cv = 3, scoring='f1')
    clf.fit(X_train,y_train)

    print('f1 =', clf.best_score_)
    print('Best params =', clf.best_params_)
    
    return clf.best_estimator_

X, y = get_train_data(df)
X_train, X_test, y_train, y_test = train_test_split(X, 
                                                    y, 
                                                    test_size=.33, 
                                                    stratify=y,
                                                    random_state=241)
clf_bin = bin_balancedrf_model(X_train, y_train)
y_pred = clf_bin.predict(X_test)
    
print('Accuracy =', accuracy_score(y_test, y_pred))
print('Balanced accuracy =', balanced_accuracy_score(y_test, y_pred))
print('F1 score =', f1_score(y_test, y_pred, average='weighted'))
    
mm_input = pd.DataFrame()
mm_input['y'] = y_test
mm_input['y_pred'] = y_pred

edges = mm_input.groupby('y')['y_pred'].value_counts()
m = pd.DataFrame(edges.unstack(level=-1))
m = m.div(m.sum(axis=1), axis=0)
a4_dims = (5, 5)
fig, ax = plt.subplots(figsize=a4_dims)
plt.title('Mismatches (ALL)')
sns.heatmap(m, annot=True, cmap='BuPu')
plt.show()