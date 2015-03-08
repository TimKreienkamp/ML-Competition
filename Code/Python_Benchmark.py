# -*- coding: utf-8 -*-
"""
Created on Thu Mar  5 16:35:38 2015

@author: timkreienkamp
"""

from sklearn.ensemble import GradientBoostingClassifier as gbt

from sklearn.cross_validation import KFold

from sklearn.preprocessing import StandardScaler

import numpy as np

import pandas as pd

#reading in the training data
train = pd.read_csv("../Data/Kaggle_Covertype_training.csv")

#subsetting the features
X = train.iloc[:,1:54].values

#X_cont = X.iloc[,0:10]
#X_factor = 

#subsetting the target
y = train.iloc[:,55].values


#create cross validation indices

cv = KFold(len(y), 5)

clf = gbt()

results = []
#cross validation loop
for traincv, testcv in cv:
    fit = clf.fit(X[traincv], y[traincv])
    preds = fit.predict(X[testcv])
    error = 1-(np.mean(preds == y[testcv]))
    results.append(error)

overall_error = np.mean(results)

print overall_error


