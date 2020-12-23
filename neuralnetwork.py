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
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPRegressor
from sklearn.model_selection import KFold
from sklearn.model_selection import LeaveOneOut
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import cross_val_predict
from sklearn.compose import make_column_transformer

# Create neural net function

def mlpr_function(data): 
    
    X = data.loc[:, ['day', 'tl']]
    y = data.loc[:, 'perf_int']

    ct = make_column_transformer(
        (StandardScaler(), ['day', 'tl']), 
        remainder='passthrough')

    neural_net_model = MLPRegressor(solver = 'lbfgs', 
                                    activation = 'tanh',
                                    hidden_layer_sizes = [20],
                                    random_state = 42,
                                    max_iter = 10000)

    nn = make_pipeline(ct, neural_net_model)
    
    loo = LeaveOneOut()
    
    #cv = KFold(20, shuffle = False)
       
    scores = cross_val_score(nn, X, y, cv=loo, scoring = 'neg_mean_absolute_error')
    MAE = abs(scores)

    predicted_values = cross_val_predict(nn, X, y, cv=loo)
    indices = data[data.performance.notna()].loc[:, ['performance']].index

    model_prediction = predicted_values[indices]    

    perf = data[data.performance.notna()].loc[:, ['performance']].to_numpy().reshape(-1)

    r = np.corrcoef(perf, model_prediction)[1,0]
    r2 = r*r
    
    return(MAE, predicted_values, r2)
    
# Apply neural net function over all subjects

nn_results = []

for i in list_of_subject_data:
    
    df = i
    loo_scores, predicted_values, r2 = mlpr_function(df)
    df['nn_predicted'] = predicted_values
    df['r2'] = r2
    
    nn_results.append(df)
    
nn_results_df = pd.concat(nn_results)

# Save results as a csv-file
df.to_csv('nn_results.csv',index=False)
    

