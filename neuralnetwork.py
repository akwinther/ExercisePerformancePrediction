# -*- coding: utf-8 -*-
"""
Created on Sun Dec 20 04:15:19 2020

@author: awi027
"""


import os
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy import optimize
from sklearn.neural_network import MLPRegressor
from sklearn.model_selection import GridSearchCV
from sklearn import preprocessing
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.model_selection import LeaveOneOut
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import cross_val_predict
from sklearn.model_selection import GroupKFold

# Cross-validation

X = np.array(subjects_df.loc[:, ['day', 'tl']])
y = np.array(subjects_df['perf_int'])
groups = np.array(subjects_df['subject'])

logo = LeaveOneGroupOut()
loo = LeaveOneOut()
#logo.get_n_splits(X, y, groups)
#gkf = GroupKFold(n_splits=6)

neural_net_model = MLPRegressor(solver = 'lbfgs', 
                                activation = 'tanh',
                                hidden_layer_sizes = [20],
                                random_state = 50,
                                max_iter = 4000)

test = make_pipeline(preprocessing.StandardScaler(), neural_net_model)
#cv = LeaveOneGroupOut().split(X, y, groups)
#cv = gkf.split(X, y, groups)

scores = cross_val_score(test, X, y, cv=loo, scoring = 'neg_mean_absolute_error')
scores


preds = cross_val_predict(test, X, y, cv=loo)


    
    
# Divide into training and testing sets

## Train model on subject 0-5, predict on subject 6 / Do cross validation


subject0 = subjects_df[subjects_df['subject'] == 0]

x_train = np.array(subject0.loc[:,['day','tl']])
min_max_scaler = preprocessing.MinMaxScaler()
x_train_minmax = min_max_scaler.fit_transform(x_train)
y = np.array(subject0.loc[:,['perf_int']]).ravel()

neural_net_model = MLPRegressor(solver = 'lbfgs', 
                                activation = 'tanh',
                                hidden_layer_sizes = [20],
                                random_state = 50,
                                max_iter = 2000)

nnfit = neural_net_model.fit(x_train_minmax, y)
nnscore = neural_net_model.score(x_train_minmax, y)
nnpred = neural_net_model.predict(x_train_minmax)
nnpred
nnscore

# Visualize

plt.plot(subject0.loc[:,['day']],preds[0:109])
#plt.plot(x, subject1.iloc[:,4])
plt.scatter(subject0.loc[:,['day']],subject0.loc[:,['performance']])


# GridSearch

mlpr = MLPRegressor(max_iter=2000)

param_list = {"hidden_layer_sizes": [(50,),(100,),(200,),(300,),(500,), (800,), (1000),], 
              "activation": ["relu"],
              "solver" : ["lbfgs"],
              "random_state" : [0,10,20,30,40,50,60,70,80,90,100]}

clf = GridSearchCV(mlpr, param_grid=param_list)

clf.fit()

# Neural net
neural_net_model = MLPRegressor(solver = 'lbfgs', 
                                activation = 'tanh',
                                hidden_layer_sizes = [10],
                                random_state = 50,
                                max_iter = 200)
                                    

nnfit = neural_net_model.fit(x_train_minmax, y)
nnscore = neural_net_model.score(x_train_minmax, y)
nnpred = neural_net_model.predict(x_train_minmax)
nnpred
nnscore

days = subject1.iloc[:,1]

plt.plot(days,nnpred)
#plt.plot(x, subject1.iloc[:,4])
plt.scatter(days,subject1.iloc[:,2])

# Test on subject 2
subject2 = subjects_list[2]

subject2_input = np.array(subject2.iloc[:,[0,1]])
subject2_input_minmax = min_max_scaler.fit_transform(subject2_input)

nnpred2 = neural_net_model.predict(subject2_input_minmax)
nnpred2

plt.plot(days,nnpred2)
#plt.plot(x, subject1.iloc[:,4])
plt.scatter(days,subject2.iloc[:,2])


for train_index, test_index in logo.split(X, y, groups=groups):
    print("TRAIN:", train_index, "TEST:", test_index)
    X_train, X_test = X[train_index], X[test_index]
    y_train, y_test = y[train_index], y[test_index]
    print(X_train, X_test, y_train, y_test)

