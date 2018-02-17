# Generating training data
n = 150; d = 200
X.tr = 0.5*matrix(runif(n*d),n,d) + matrix(rep(0.5*runif(n),d),n,d)
# Generating response
y.tr = -2*sin(X.tr[,1]) + X.tr[,2]^2-1/3 + X.tr[,3]-1/2 + exp(-X.tr[,4]) + exp(-1)-1

# Generating testing data
n = 500; d = 200
X.te = 0.5*matrix(runif(n*d),n,d) + matrix(rep(0.5*runif(n),d),n,d)
# Generating response
y.te = -2*sin(X.te[,1]) + X.te[,2]^2-1/3 + X.te[,3]-1/2 + exp(-X.te[,4]) + exp(-1)-1


#Functions

rmse = function(x,y){return(sqrt(mean((x-y)**2)))}

#Backfitting using smoothing splines

# alpha 
alpha = mean(y.tr)
tol = 1e-6
max_iter = 800
backfit = function(tol,alpha,train_labels,train_data,max_iter,lam) {
  
  diff <- Inf
  fhat <-matrix(0,nrow = nrow(train_data),ncol = ncol(train_data))
  y <- train_labels
  iter = 0
  #initialize rss0
  df_vector = rep(NA,ncol(train_data))
  norm_vector = rep(NA,ncol(train_data))
  #for (i in 1:dim(fhat)[2]){fhat[,i] = fns[[i]](train_data[,i])}
  while(diff>0 & iter < max_iter){
    
    rss0 = sum((y-alpha-rowSums(fhat))**2)
    for (j in  1:ncol(fhat)){
      tmp_dat = as.matrix(train_data[,-j])
      #test_dat = as.matrix(test_data[,-j])
      val = 0
      for(col in 1:ncol(tmp_dat)){
        sp_fit = smooth.spline(tmp_dat[,col],y)
        preds = predict(sp_fit,tmp_dat[,col])$y
        val=val+preds}
      
      residuals = y-alpha-val
      sp_fit_1 = smooth.spline(train_data[,j],residuals)
      df_vector[j] = sp_fit_1[10]$df
      f = predict(sp_fit_1,train_data[,j])$y
      
      norm_f = sqrt(mean(f^2))
      val_f = (1-(lam/norm_f))
      norm_vector[j] = norm_f
      if (val_f > 0) {f = f*val_f}
      else {f = rep(0,length(f))}
      f = f-mean(f)
      fhat[,j] = f
    }
    rss = sum((y-alpha-rowSums(fhat))**2)
    diff = abs(rss-rss0)-(tol*rss)
    iter = iter +1 
  }
  
  y_hat_tr = 0
  
  for(col in 1:ncol(fhat)){
    y_hat_tr =y_hat_tr +fhat[,col]
  }
  y_hat_tr = y_hat_tr + alpha
  
  lst = list(fhat,y_hat_tr,df_vector,norm_vector)
  return(lst)
}


GCV = function(tol,alpha,y.tr,X.tr,max_iter){
  lam_values = seq(11.5,12.5,0.2)
  err_val = length(lam_values)
  for (lam in 1:length(lam_values)) {
    r = backfit(tol,alpha,y.tr,X.tr,max_iter,lam_values[lam])
    dof = degree_of_freedom(r[[1]],r[[3]])
    err = rmse(r[[2]],y.tr)**2
    err_val[lam]=err/(1-(dof/length(y.tr)))**2
  }
  return(list(err_val,lam_values))
}


degree_of_freedom = function(fhat,deg_vector){
  for (i in 1:ncol(fhat)){
    if (sum(fhat[,i] == 0) > 0){
      deg_vector[i] = 0
    }
  }
  return(sum(deg_vector))
}



predict_test = function(alpha,X.tr,y.tr,X.te){
  sav = 0
  for (i in 1:ncol(X.tr)){
    fit1 = smooth.spline(X.tr[,i],y.tr)
    sav = sav + (predict(fit1,X.te[,i])$y-mean(predict(fit1,X.te[,i])$y))
    
  }
  return(sav+alpha)
}




# gcv score
obj = GCV(tol,alpha,y.tr,X.tr,max_iter) #lambda optimum = 12.5
# lambda = 12.5
r = backfit(tol,alpha,scale(y.tr),scale(X.tr),max_iter,12.5)
yhat.tr = r[[2]]
rmse(y.tr,yhat.tr) # 2.40

#Test error and predictions
yhat.te = predict_test(alpha,scale(X.tr),scale(y.tr),X.te)
rmse(y.te,yhat.te)

#no of variablese remaining 
idx = 0
for (i in 1:ncol(r[[1]])){
  if (sum(r[[1]][,i] == 0) > 0){
    idx = idx +1
  }
}
#no of variables left  = dim(X.tr)[2] - idx




#samQL results
library(SAM)
library(splines)
sparse_fit = samQL(X.tr,y.tr)
sparse_fit$sse[30]
sparse_fit$lambda[30]
yhat.tr.samql = predict.samQL(sparse_fit,X.tr)$values[4351:4500]
rmse(yhat.tr.samql,y.tr)

#Plots
par(mfrow = c(2,1))
plot(X.tr[,1],y.tr,main = "SpAM fit")
lines(X.tr[,1],yhat.tr)

plot(X.tr[,1],yhat.tr.samql,main = "SpAM Fit- samQL results")
lines(X.tr[,1],yhat.tr.samql)


