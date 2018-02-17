
# coding: utf-8

# In[14]:

import os
import numpy as np
import pandas as pd

'''Read Data'''
def read_data():
    data = pd.read_csv('tempo_hwdata.csv',encoding='utf8')
    data = data.iloc[:,1:]
    train_data = data.iloc[0:13,1:]
    test_data = data.iloc[13:,1:]
    return train_data,test_data

'''Fill Missing Values'''
def fill_nas(train_data,test_data):
    '''Load the Data and Fill the NAS'''
    x_variables_train = train_data.iloc[:,5:]
    y_variable_train = train_data.iloc[:,0]
    x_variables_train = x_variables_train.fillna(x_variables_train.mean(axis=0),inplace=True)
    x_variables_test = test_data.iloc[:,5:]
    x_variables_test = x_variables_test.fillna(x_variables_test.mean(axis=0),inplace=True)
    return x_variables_train,y_variable_train,x_variables_test

'''Calculate Error Function'''
def loss_fun(preds,labels):
    factors=[1,2,3,0.5,0.33]
    loss_scores=[]
    for fac in factors:
        loss_scores.append(abs((preds*fac)-labels))

    loss_df=pd.DataFrame(loss_scores).T
    min_score=[]
    for i in range(loss_df.shape[0]):
        min_score.append(np.min(loss_df.iloc[i,:].values))
    min_score=np.array(min_score)
    err_fac=[0.04,-0.04]
    err_list=[]
    for val in err_fac:
        err_list.append(min_score+min_score*val)
        
    err_df=pd.DataFrame(err_list).T
    err_score=[]
    for i in range(err_df.shape[0]):
        err_score.append(np.min(err_df.iloc[i,:].values))
    return np.sqrt(np.mean(np.array(err_score)**2))

'''Find Multicollinearity'''
def find_corr(orig_data):
    another_matrix=orig_data.iloc[:,5:]
    corr=another_matrix.corr()
    corr_matrix=np.matrix(corr.values)
    return np.mean(np.triu(corr_matrix))

'''Prediction'''
def predict(model,test_data):
    return model.predict(test_data)


# In[2]:

data = pd.read_csv('tempo_hwdata.csv',encoding='utf8', index_col=0)
data.fillna(np.mean(data, axis=0),inplace=True)
print ('Average Correlation among the Predictor Variable is')
find_corr(data)


# In[3]:

train_data,test_data = read_data()
features, correct_answer, test = fill_nas(train_data,test_data)


# In[23]:

'''Ridge Regression'''
alpha = np.arange(630,640,0.005)
from sklearn import linear_model
model= linear_model.RidgeCV(cv=3).fit(features, correct_answer)
print ('The Overall Error in Ridge Regression Using Cross Validation is this')
loss_fun(model.predict(test),data.iloc[13:,1])


# In[24]:

'''Lasso'''
alpha = np.arange(630,640,0.005)
from sklearn import linear_model
model= linear_model.LassoCV(cv=3).fit(features, correct_answer)
print ('The Overall Error in Losso Regression Using Cross Validation is this')
loss_fun(model.predict(test),data.iloc[13:,1])


# In[31]:

from sklearn import linear_model
lr = linear_model.Lasso(normalize=True,fit_intercept=True)
lr.fit(features,correct_answer)


# In[32]:

'''You can give the test Data Set for Prediction as a Parameter in Function Call below'''
predict(lr,test)

