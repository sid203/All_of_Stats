library(wrassp)
library(signal)
library(tuneR)
library(ranger)
library(e1071)
library(caret)
library(randomForest)

read_file = function(index){
  
  path_file='hw2data/f'
  f=paste(c(path_file,index),collapse = "")
  name=paste(c(f,".au"),collapse = "")
  x <- read.AsspDataObj(name)
  return(x)
}


gen_features=function(){
  indices=1:150
  winsize <- 2048 # time-windowing (in number of samples, power of 2 for the FFT)
  hopsize <- 512 # windows overlap (in number of samples)
  noverlap <- winsize - hopsize
  corrtime <- 15 
  nfft <- 2048
  features.mat = matrix(NA,nrow = 150,ncol = 656*8)
  dimensions = c(NA,150)
  for (i in 1:150){
    x = read_file(i)
    xwv <- Wave( as.numeric(x$audio), samp.rate = rate.AsspDataObj(x), bit = 16)
    xw.dwn = downsample(xwv, samp.rate = 11025)
    fs <- xw.dwn@samp.rate # sampling rate
    sp <- specgram(x = xw.dwn@left, n = nfft, Fs = fs, window = winsize, overlap = noverlap)
    ntm <- ncol(sp$S) # number of (overlapping) time segments in the STFT
    energy=cal_energy(fs,ntm,sp)
    feat.vec <- c(energy)
    #dimensions[i] = dim(energy)[2]
    features.mat[i,1:length(feat.vec)]=feat.vec
  }
  
  return(features.mat)
}

cal_energy=function(fs,ntm,sp){
  lowB <- 100
  nb <- 2^3
  nfft <- 2048
  eps <- .Machine$double.eps # machine precision to avoid over/underflow
  fco <- round( c(0, lowB*(fs/2/lowB)^((0:(nb-1))/(nb-1)))/fs*nfft )
  energy <- matrix(0, nb, ntm)
  for (tm in 1:ntm){
    for (i in 1:nb){
      lower_bound <- 1 + fco[i]
      upper_bound <- min( c( 1 + fco[i + 1], nrow(sp$S) ) )
      energy[i, tm] <- sum( abs(sp$S[ lower_bound:upper_bound, tm ])^2 )
    }
  }
  energy[energy < eps] <- eps
  energy = 10*log10(energy)
  return(energy)
}

labels = read.table('hw2data/Labels.txt') #labels
features = as.data.frame(gen_features()) #feature matrix





# Fill missing values ------------------------------------------------

nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}
#names = nacols(features)

#for ( i in 1:length(names)){
#  col = as.numeric(strsplit(names[i],'V')[[1]][2])
#  features[,col] = na.spline(features[,col])
#}

features_df = features[ , colSums(is.na(features)) == 0] #drop missing values
features_df = cbind.data.frame(features_df,labels)
# Train and Test splitting ------------------------------------------------


set.seed(107)
inTrain = createDataPartition(y = features_df$x,
                              p = .75,
                              list = FALSE)
head(inTrain)
training = features_df[ inTrain, ]
testing = features_df[-inTrain, ]

# Random Forest ---------------------------------------------------------------------


rf = randomForest(x ~ ., data = training,
                  importance = T)

yhat.rf = predict(rf,newdata=testing[,-5249])

round( mean( yhat.rf != testing$x )*100, 3)
confusionMatrix(yhat.rf,testing$x)

# Something with Ranger ---------------------------------------------------
library(ranger)
rang3 <- ranger(x ~ .,
                write.forest=TRUE,
                data=training) 
yhat.rf.ranger= predict(rang3,data=testing[,-5249])
round( mean( yhat.rf.ranger$predictions != testing$x )*100, 3)
confusionMatrix(yhat.rf.ranger$predictions,testing$x)
# SVM ---------------------------------------------------------------------

svmfit=svm(x~ ., data = training, kernel="sigmoid", cost=0.5,scale=TRUE, cross = 10)
yhat.rf = predict(svmfit,newdata=testing[,-5249])
round( mean( yhat.rf != testing$x )*100, 3)
confusionMatrix(yhat.rf,testing$x)

# grid search cv to tune the parameters
#svm_tune <- tune(svm, x~ ., data = training, kernel="radial", ranges=list(cost=2^(-1:5), gamma=c(.5,1,2)))
#bestmod=svm_tune$best.model
#summary(bestmod)
#bestmod$cost
#bestmod$kernel
#bestmod$degree
#bestmod$gamma


# Conclusion --------------------------------------------------------------

#So far best predictions have been obtained with ranger with accuracy of 62 % 
# We tried to fill thie missing values with spline fit, but results were not good on that. 
# So we dropped all the columns having nans and this is the best accuracy we have got so far. 

#Results of ranger
yhat.rf.ranger= predict(rang3,data=testing[,-5249])
round( mean( yhat.rf.ranger$predictions != testing$x )*100, 3)
confusionMatrix(yhat.rf.ranger$predictions,testing$x)
