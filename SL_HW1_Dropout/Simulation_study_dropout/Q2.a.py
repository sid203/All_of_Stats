
# coding: utf-8

# In[25]:

import numpy as np
import pandas as pd
import os
from sklearn import linear_model


'''Artificial Noise'''
def add_noise(delta,f):
    list_of_dataframes=[]
    features_used = f.copy()
    artificial_error=np.random.binomial(1,delta,features_used.shape[0]) / ((1-(delta)))
    for i in range(features_used.shape[1]):
        features_used.iloc[:,i]= artificial_error * features_used.iloc[:,i]
    
    return features_used

'''Fit the model and calculate error'''
def calculate_error(X,y):
    reg = linear_model.LinearRegression(normalize= True,fit_intercept=True)
    reg.fit(X,y)
    preds=reg.predict(X)
    
    return np.mean(np.square(preds - y))

''''''
def delta_run(delta_vec,m,features,response):
    df=pd.DataFrame(columns=['delta_values','values_of_m','errors'])
    m_errors=[]
    m_values=[]
    delta_values=[]
    
    for delta in delta_vec:
        for i in range(m):
            m_errors.append(calculate_error(add_noise(delta,features).values,response))
            m_values.append(i)
            delta_values.append(delta)
            
    df.iloc[:,0]=delta_values
    df.iloc[:,1]=m_values
    df.iloc[:,2]=m_errors
    return df


# In[26]:

'''Load the Data'''
data = pd.read_csv('supernova.csv',encoding='utf8')
data = data.iloc[:,1:]
data.set_index('SN',inplace=True)

'''Train1 and Test1'''
train_1 = data.iloc[:25,:10]
response_1 = data.iloc[:25,10]
test_1 = data.iloc[25:,:10]
y_1 = data.iloc[25:,10]


'''Train2 and Test2'''
train_2 = data.iloc[14:,:10]
response_2 = data.iloc[14:,10]
test_2 = data.iloc[:14,:10]
y_2 = data.iloc[:14,10]


'''Train3 and Test3'''
train_3 = data.iloc[19:,:10]
response_3 = data.iloc[19:,10]
test_3 = data.iloc[:19,:10]
y_3 = data.iloc[:19,10]


'''Train4 and Test4'''
train_4 = pd.concat([data.iloc[:9,:10],data.iloc[23:,:10]])
response_4 = pd.concat([data.iloc[:9,10],data.iloc[23:,10]])
test_4 = data.iloc[9:23,:10]
y_4 = data.iloc[9:23,10]


# In[23]:

'''Train 1 and Test 1'''
delta_arr=np.arange(0,1,0.1)
train_df_1=delta_run(delta_arr,1000,train_1,response_1)
test_df_1=delta_run(delta_arr,1000,test_1,y_1)

'''Train 2 and Test 2'''
delta_arr=np.arange(0,1,0.1)
train_df_2=delta_run(delta_arr,1000,train_2,response_2)
test_df_2=delta_run(delta_arr,1000,test_2,y_2)

'''Train 3 and Test 3'''
delta_arr=np.arange(0,1,0.1)
train_df_3=delta_run(delta_arr,1000,train_3,response_3)
test_df_3=delta_run(delta_arr,1000,test_3,y_3)

'''Train 4, Test 4'''
delta_arr=np.arange(0,1,0.1)
train_df_4=delta_run(delta_arr,1000,train_4,response_4)
test_df_4=delta_run(delta_arr,1000,test_4,y_4)


# In[16]:

tr_df=(test_df_1+test_df_2+test_df_3+test_df_4)/4
tr_df.loc[tr_df.errors==np.min(tr_df.errors)]


# In[49]:

'''Ridge Regression'''
from sklearn.linear_model import Ridge 
ridge=Ridge(normalize=True)
ridge.fit(train_1,response_1)
preds_ridge=ridge.predict(test_1)
error_ridge1=np.mean(np.square(preds_ridge-y_1))

ridge.fit(train_2,response_2)
preds_ridge=ridge.predict(test_2)
error_ridge2=np.mean(np.square(preds_ridge-y_2))

ridge.fit(train_3,response_3)
preds_ridge=ridge.predict(test_3)
error_ridge3=np.mean(np.square(preds_ridge-y_3))

ridge.fit(train_4,response_4)
preds_ridge=ridge.predict(test_1)
error_ridge4=np.mean(np.square(preds_ridge-y_4))

error_ridge1= (error_ridge1+error_ridge2+error_ridge3+error_ridge4)/4


# In[48]:

'''Lasso Regression'''
from sklearn.linear_model import Lasso
lasso=Lasso(normalize=True)
lasso.fit(train_1,response_1)
preds_lasso=lasso.predict(test_1)
error_lasso1=np.mean(np.square(preds_lasso-y_1))

lasso.fit(train_2,response_2)
preds_lasso=lasso.predict(test_2)
error_lasso2=np.mean(np.square(preds_lasso-y_2))

lasso.fit(train_3,response_3)
preds_lasso=lasso.predict(test_3)
error_lasso3=np.mean(np.square(preds_lasso-y_3))

lasso.fit(train_4,response_4)
preds_lasso=lasso.predict(test_4)
error_lasso4=np.mean(np.square(preds_lasso-y_4))

error_lasso1=(error_lasso1+error_lasso2+error_lasso3+error_lasso4)/4


# In[47]:

print ('Average Error Comparison of Dropout,Ridge, and Lasso respectively')
print(np.mean(np.mean(tr_df)),error_ridge1,error_lasso1) #Delta=0.9 M=194 Error=0.272328

print ('Minimum Error Comparison of Drouput,Ridge and Lasso')
print (tr_df.loc[tr_df.errors==np.min(tr_df.errors)].iloc[0,2])
print (np.min(np.array([error_lasso1,error_lasso2,error_lasso3,error_lasso4])))
print (np.min(np.array([error_ridge1,error_ridge2,error_ridge3,error_ridge4])))

print ('Maximum Error Comparison of Drouput,Ridge and Lasso')
print (tr_df.loc[tr_df.errors==np.max(tr_df.errors)].iloc[0,2])
print (np.max(np.array([error_lasso1,error_lasso2,error_lasso3,error_lasso4])))
print (np.max(np.array([error_ridge1,error_ridge2,error_ridge3,error_ridge4])))

