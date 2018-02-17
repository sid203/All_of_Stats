load('ore.RData')
#response <- ore$width
#data <- as.matrix(ore[,1:2])

train_data <- as.matrix(ore[1:22,1:2])
train_labels <-ore$width[1:22]
#train_data <- as.matrix(ore[,1:2])
#train_labels <-ore$width

test_data <- as.matrix(ore[23:38,1:2])
test_labels <-ore$width[23:38]

s#functions
rmse = function(x,y){return(sqrt(mean((x-y)**2)))}

# some smoothing functions
k.epan<-function(x) {
  a <- 0.75*(1-x^2)
  tmp <- a*(abs(x)<=1)
  return(tmp)
}


kernel = function(x,y,sigma=1){ return (exp(-(x-y)^2) / 2*(sigma**2)) }
smooth = function(Y ,X,x_test = 'not pres'){
  
  if (x_test =='not pres'){
    X_new = rep(NA,length(X))
    for (i in 1:length(X)){
      tmp = kernel(X[i],X) #gaussian smoother
      tmp = tmp/sum(tmp)
      X_new[i]=sum(Y*tmp)
      
      
    }
    return(X_new)
  }
    
  else{
    X_new_1 = rep(NA,length(x_test))
    for (i in 1:length(x_test)){
      tmp_1 = kernel(x_test[i],X) #gaussian smoother
      tmp_1 = tmp_1/sum(tmp_1)
      X_new_1[i]=sum(Y*tmp_1)
    }
    return(X_new_1)
    
  }
}


# Smoother : smooth.spline ------------------------------------------------


#Backfitting using smoothing splines

# alpha 
alpha = mean(train_labels)
tol = 1e-6
max_iter = 800
backfit = function(tol,alpha,train_labels,train_data,max_iter) {
  
  diff <- Inf
  fhat <-matrix(0,nrow = nrow(train_data),ncol = ncol(train_data))
  y <- train_labels
  iter = 0
  #initialize rss0
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
      f = predict(sp_fit_1,train_data[,j])$y
      f = f-mean(f)
      fhat[,j] = f
    }
    rss = sum((y-alpha-rowSums(fhat))**2)
    diff = abs(rss-rss0)-(tol*rss)
    iter = iter +1 
  }
  lst = list(fhat,rss,diff)
  return(lst)
}

r = backfit(tol,alpha,train_labels,train_data,max_iter)
#Train set predictions
y_hat_train = r[[1]][,2]+r[[1]][,1]+alpha
rmse(y_hat_train,train_labels) # 1.60

fit1 = smooth.spline(train_data[,1],train_labels)
pred1 = predict(fit1,test_data[,1])$y-mean(predict(fit1,test_data[,1])$y)

fit2 = smooth.spline(train_data[,2],train_labels)
pred2 = predict(fit2,test_data[,2])$y - mean(predict(fit2,test_data[,2])$y)

#test set predictions 
y_hat_test = alpha + pred1 + pred2
rmse(y_hat_test,test_labels) #3.19



#mgcv results
library(mgcv)
par(mfrow = c(2,2))
ore.gam <- gam(width ~ s(t1) + s(t2), data = ore)
plot(ore.gam, pages = 1)

plot(ore$t1,ore$width,main = "mgcv fit", xlab = "t1", ylab = "width")
lines(ore$t1,ore.gam[3]$fitted.values)
plot(ore$t2,ore$width,main = "mgcv fit", xlab = "t2", ylab = "width")
lines(ore$t2,ore.gam[3]$fitted.values)


plot(train_data[,1],train_labels,main = "Additive Model - Spline smoother", xlab = "t1", ylab = "width")
lines(train_data[,1],y_hat_train)

plot(train_data[,2],train_labels,main = "Additive Model - Spline smoother", xlab = "t2", ylab = "width")
lines(train_data[,2],y_hat_train)


# Smoother : Gaussian Kernel ----------------------------------------------

#Backfitting using Kernel regression
# alpha 
alpha = mean(train_labels)
tol = 1e-6
max_iter = 800
backfit_kernel = function(tol,alpha,train_labels,train_data,max_iter) {
  
  diff <- Inf
  fhat <-matrix(0,nrow = nrow(train_data),ncol = ncol(train_data))
  y <- train_labels
  iter = 0
  #initialize rss0
  #for (i in 1:dim(fhat)[2]){fhat[,i] = fns[[i]](train_data[,i])}
  while(diff>0 & iter < max_iter){
    
    rss0 = sum((y-alpha-rowSums(fhat))**2)
    for (j in  1:ncol(fhat)){
      tmp_dat = as.matrix(train_data[,-j])
      val = 0
      for(col in 1:ncol(tmp_dat)){
        val =val +  smooth(y,tmp_dat[,col])}
      
      residuals = y-alpha-val
      f = smooth(residuals,train_data[,j])
      f = f-mean(f)
      fhat[,j] = f
    }
    rss = sum((y-alpha-rowSums(fhat))**2)
    diff = abs(rss-rss0)-(tol*rss)
    iter = iter +1 
  }
  lst = list(fhat,rss,diff)
  return(lst)
}

r_kernel = backfit_kernel(tol,alpha,(train_labels),(train_data),max_iter)
y_hat_train_kernel = r_kernel[[1]][,1]+r_kernel[[1]][,2] + alpha
rmse(y_hat_train_kernel,train_labels) # 2.12

#test set predictions 
cov_1 = smooth(train_labels,train_data[,1],scale(test_data[,1]))
cov_1 = cov_1 - mean(cov_1)

cov_2 = smooth(train_labels,train_data[,2],scale(test_data[,2]))
cov_2 = cov_2 - mean(cov_2)

y_hat_test_kernel = cov_1 + cov_2 + alpha
rmse(y_hat_test_kernel,test_labels) # 4.55

#mgcv results
library(mgcv)
par(mfrow = c(2,2))

plot(ore$t1,ore$width,main = "mgcv fit", xlab = "t1", ylab = "width")
lines(ore$t1,ore.gam[3]$fitted.values)
plot(ore$t2,ore$width,main = "mgcv fit", xlab = "t2", ylab = "width")
lines(ore$t2,ore.gam[3]$fitted.values)


plot(train_data[,1],train_labels,main = "Additive Model- Kernel smoother", xlab = "t1", ylab = "width")
lines(train_data[,1],y_hat_train_kernel)

plot(train_data[,2],train_labels,main = "Additive Model - Kernel smoother", xlab = "t2", ylab = "width")
lines(train_data[,2],y_hat_train_kernel)





