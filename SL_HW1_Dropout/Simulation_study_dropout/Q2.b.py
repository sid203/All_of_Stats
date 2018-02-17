
# coding: utf-8

# In[294]:

import numpy as np
import pandas as pd
import os
import random
from sklearn import linear_model
'''Generate Feature Vector'''
def generate_feature_set():
    feature_set = pd.DataFrame(np.zeros((75, 100)))
    for i in range(0,75):
        feature_set.iloc[i,:] = np.random.uniform(1,100,100)
    return feature_set

'''Generate response'''
def generate_response(mean):
    return np.random.normal(mean * 100,10,75)

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


# In[277]:

'''Generate Data Set'''
feature_set =generate_feature_set()
var = np.var(feature_set.var(axis=0))
mean = np.mean(np.mean(feature_set))
response = generate_response(mean,var)
response = pd.DataFrame(response)


# In[285]:

'''Split the Data Set in five Training and Test Ones'''
train_set_1 = feature_set.iloc[:60,:]
test_set_1 = feature_set.iloc[60:75]
train_set_2 = feature_set.iloc[15:75,:]
test_set_2 = feature_set.iloc[:15,:]
train_set_3 = pd.concat([feature_set.iloc[:15,:],feature_set.iloc[30:,:]])
test_set_3 = feature_set.iloc[15:30,:]
train_set_4 = pd.concat([feature_set.iloc[:30,:],feature_set.iloc[45:,:]])
test_set_4 = feature_set.iloc[30:45,:]
train_set_5 = pd.concat([feature_set.iloc[:45,:],feature_set.iloc[60:,:]])
test_set_5 = feature_set.iloc[45:60,:]


# In[296]:

'''Apply Ridge Regression and Calculate MSE for all five Test Data Sets'''
from sklearn.linear_model import Ridge 
ridge=Ridge(normalize=True)
ridge.fit(train_set_1,response[:60])
preds_ridge=ridge.predict(test_set_1)
error_ridge1=np.mean(np.square(preds_ridge-response.iloc[60:75,:]))

ridge.fit(train_set_2,response[15:75])
preds_ridge=ridge.predict(test_set_2)
error_ridge2=np.mean(np.square(preds_ridge-response.iloc[:15,:]))

ridge.fit(train_set_3,pd.concat([response.iloc[:15,:],response.iloc[30:,:]]))
preds_ridge=ridge.predict(test_set_3)
error_ridge3=np.mean(np.square(preds_ridge-response.iloc[15:30,:]))

ridge.fit(train_set_4,pd.concat([response.iloc[:30,:],response.iloc[45:,:]]))
preds_ridge=ridge.predict(test_set_4)
error_ridge4=np.mean(np.square(preds_ridge-response.iloc[30:45,:]))

ridge.fit(train_set_5,pd.concat([response.iloc[:45,:],response.iloc[60:,:]]))
preds_ridge=ridge.predict(test_set_5)
error_ridge5=np.mean(np.square(preds_ridge-response.iloc[45:60,:]))

tot_ridge = (error_ridge1+error_ridge2+error_ridge3+error_ridge4+error_ridge5)/5
print ('Total average Error on five test Data Sets Using Ridge Regression is this')
tot_ridge


# In[295]:

'''Apply Lasso Regression and Calculate MSE for all five Test Data Sets'''
from sklearn.linear_model import Lasso 
lasso=Lasso(normalize=True)
lasso.fit(train_set_1,response.iloc[:60,0])
preds_lasso=lasso.predict(test_set_1)
error_lasso1=np.mean(np.square(preds_lasso-response.iloc[60:75,0]))

lasso.fit(train_set_2,response.iloc[15:75,0])
preds_lasso=lasso.predict(test_set_2)
error_lasso2=np.mean(np.square(preds_lasso-response.iloc[:15,0]))

lasso.fit(train_set_3,pd.concat([response.iloc[:15,0],response.iloc[30:,0]]))
preds_lasso=lasso.predict(test_set_3)
error_lasso3=np.mean(np.square(preds_lasso-response.iloc[15:30,0]))

lasso.fit(train_set_4,pd.concat([response.iloc[:30,0],response.iloc[45:,0]]))
preds_lasso=lasso.predict(test_set_4)
error_lasso4=np.mean(np.square(preds_lasso-response.iloc[30:45,0]))

lasso.fit(train_set_5,pd.concat([response.iloc[:45,0],response.iloc[60:,0]]))
preds_lasso=lasso.predict(test_set_5)
error_lasso5=np.mean(np.square(preds_lasso-response.iloc[45:60,0]))

tot_lasso = (error_lasso1+error_lasso2+error_lasso3+error_lasso4+error_lasso5)/5
print ('Total Average Error on five test Data Sets Using Lasso Regression is this')
tot_lasso


# In[298]:

'''Train 1 and Test 1'''
delta_arr=np.arange(0,1,0.1)
train_df_1=delta_run(delta_arr,100,train_set_1,response.iloc[:60,0])
test_df_1=delta_run(delta_arr,100,test_set_1,response.iloc[60:75,0])
'''Train 2 and Test 2'''
delta_arr=np.arange(0,1,0.1)
train_df_2=delta_run(delta_arr,100,train_set_2,response.iloc[15:75,0])
test_df_2=delta_run(delta_arr,100,test_set_2,response.iloc[:15,0])
'''Train 3 and Test 3'''
delta_arr=np.arange(0,1,0.1)
train_df_3=delta_run(delta_arr,100,train_set_3,pd.concat([response.iloc[:15,0],response.iloc[30:,0]]))
test_df_3=delta_run(delta_arr,100,test_set_3,response.iloc[15:30,0])
'''Train 4 and Test 4'''
delta_arr=np.arange(0,1,0.1)
train_df_4=delta_run(delta_arr,100,train_set_4,pd.concat([response.iloc[:30,0],response.iloc[45:,0]]))
test_df_4=delta_run(delta_arr,100,test_set_4,response.iloc[30:45,0])
'''Train 5 and Test 5'''
delta_arr=np.arange(0,1,0.1)
train_df_5=delta_run(delta_arr,100,train_set_5,pd.concat([response.iloc[:45,0],response.iloc[60:,0]]))
test_df_5=delta_run(delta_arr,100,test_set_5,response.iloc[45:60,0])
te_df=(test_df_1+test_df_2+test_df_3+test_df_4+test_df_5)/5
min_error = te_df.loc[te_df.errors==np.min(te_df.errors)]


# In[300]:

print ('Average Error Comparison of Dropout,Ridge, and Lasso respectively')
print(np.mean(np.mean(te_df)),tot_ridge,tot_lasso) 

print ('Minimum Error Comparison of Drouput,Ridge and Lasso')
print (te_df.loc[te_df.errors==np.min(te_df.errors)].iloc[0,2])
print (np.min(np.array([error_lasso1,error_lasso2,error_lasso3,error_lasso4,error_lasso5])))
print (np.min(np.array([error_ridge1,error_ridge2,error_ridge3,error_ridge4,error_ridge5])))

print ('Maximum Error Comparison of Drouput,Ridge and Lasso')
print (te_df.loc[te_df.errors==np.max(te_df.errors)].iloc[0,2])
print (np.max(np.array([error_lasso1,error_lasso2,error_lasso3,error_lasso4,error_lasso5])))
print (np.max(np.array([error_ridge1,error_ridge2,error_ridge3,error_ridge4,error_ridge5])))

