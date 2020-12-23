# -*- coding: utf-8 -*-
"""
Created on Fri Dec 18 20:03:40 2020

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


os.chdir(r'C:\Users\awi027\OneDrive - UiT Office 365\Documents\PerformancePredicitionInSport')

subjects_data = pd.read_excel('Busso2017su1.xlsx')

training_cols = subjects_data.iloc[:, 1:13:2]
performance_cols = subjects_data.iloc[:, 2:13:2]

list_of_subject_data = []

for i in range(0,6):
    data = {'day' : subjects_data.iloc[:, 0], 
            'tl' : training_cols.iloc[:, i], 
            'performance' : performance_cols.iloc[:, i]}
    
    df = pd.DataFrame(data, columns = ['day', 'tl', 'performance'])
    df['week_num'] = np.repeat(np.arange(0, 16), 7)[0:len(df['day'])]
    df['day_in_week'] = np.array(list(np.arange(1,8))*16)[0:len(df['day'])]
    df['perf_int'] = df['performance'].interpolate(method = "cubic").fillna(method = "bfill").fillna(method = "ffill")
    df['subject'] = str('subject') + str(i+1)
    phase_conditions = [
        (df['week_num'] == 0),
        (df['week_num'] >= 1) & (df['week_num'] <= 8),
        (df['week_num'] >= 10) & (df['week_num'] <= 13)]
    phase_choices = ['pre', 'first', 'second']
    df['phase'] = np.select(phase_conditions, phase_choices, default='rest')
    session_conditions = [
        (df['phase'] == 'second') & (df['day_in_week'] == 2),
        (df['phase'] == 'second') & (df['day_in_week'] == 4),
        (df['tl'] == 100),
        (df['tl'] == 0)]
    session_choices = ['5x5', '5x5', 'trial', 'rest_day']
    df['session'] = np.select(session_conditions, session_choices, default='4x5+trial')
    
    df = df[['subject', 'day', 'week_num', 'day_in_week', 'phase', 'session', 'tl', 'performance', 'perf_int']]
    
    list_of_subject_data.append(df)

#subjects_df = pd.concat(subjects_list)