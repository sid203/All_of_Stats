library(mcmc)
library(coda)
library(mcmcse)
# Simulating the Dirichlet Process ----------------------------------------

# Stick Breaking representation  ------------------------------------------

G0 = function(n){return(rnorm(n,0,1))} #random points on the real line

draw_betas = function(n,M){return(rbeta(n,1,M))}
stick_breaking = function(nn = 10,M = 2,thetas = G0(nn)){
  
  
  betas = draw_betas(nn,M)
  
  weights = c()
  weights[1] = betas[1]
  
  for(i in 2:nn){
    weights[i] = betas[i] * prod(1 - betas[1:(i-1)])
  }
  
  theta <- sample(thetas, prob = weights, replace = TRUE)
  df = data.frame(weights,theta)
  return(df)
}

xx = stick_breaking(nn = 100, M = 1 , G0(100))
#png(filename="stick_breaking_M_1.png")
plot(x = xx[,2],y = xx[,1],type ='h',main = paste('Stick Breaking with M = 1 and G0 = N(0,1)'))
#dev.off()

#plots the cdfs and generates them
gen_cdf_mean = function(runs = 10, nn = 100, M = 50,theta = thetas ){
  
  weights_df = data.matrix(matrix(ncol = nn, nrow = runs))
  samples_df = data.matrix(matrix(ncol = nn, nrow = runs))
  for(i in 1:runs)
  {
    out = stick_breaking(nn , M, theta)
    samples = out[,2]
    samples_df[i,] = samples
    weights = out[,1]
    weights_df[i,] = cumsum(weights)
    #plot((samples),(weights), type = 'h')
  }
  
  matplot(sort(samples),t(weights_df),col = 1:5,type = 'b')
  curve(pnorm(x),add = T, title(paste('M = ',M)))
  
  means = apply(weights_df,2,mean)
  variance = apply(weights_df,2,var)
  return(list(weights_df,means,variance))
}



#Realizations of DP for different values of M , along with prior mean and prior variances
# M = 5 -------------------------------------------------------------------
lst = gen_cdf_mean(runs = 10, nn = 100,M=5, theta = thetas)
wt = lst[[1]]
means = lst[[2]]     #this mean vector will hold the mean of generated realizations from DP i.e. E[G(B)]
variance = lst[[3]] 
plot(means)
plot(variance)


# M = 25 ------------------------------------------------------------------
lst = gen_cdf_mean(runs = 10, nn = 100,M=25, theta = thetas)
wt = lst[[1]]
means = lst[[2]]
variance = lst[[3]]
plot(means)
plot(variance)

# M = 50 ------------------------------------------------------------------

lst = gen_cdf_mean(runs = 10, nn = 100,M=50, theta = thetas)
wt = lst[[1]]
means = lst[[2]]
variance = lst[[3]]
plot(means)
plot(variance)


# Ferguson's definition ---------------------------------------------------

require(MCMCpack)

sample.dir<-function(nn=10, M=2, ln = 11){
  x <- seq(-4,4, length=ln)
  y<-c()
  y[1]<-pnorm(x[1])
  for(i in 2:ln) y[i]<-pnorm(x[i])-pnorm(x[(i-1)])
  y<-c(y, 1-pnorm(x[ln]))
  param<-M*y
  #return(param)
  sample.dir<-rdirichlet(nn,param)
  draw<-apply(t(sample.dir), 2, cumsum)
  return(draw)}



# M = 5 -------------------------------------------------------------------

draws<-sample.dir(nn = 100, M = 5 , ln = 10)
xx<-c(seq(-4,4, length=10),5)
matplot(xx, draws, col=1:10, type="b")
curve(pnorm(x), add=T)
mean_vec = apply(draws,2,mean) #vector to hold values of mean for all the sets B1,B2,.....Bk, first element is the mean of observations
var_vec = apply(draws,2,var)
plot(mean_vec)
plot(var_vec)





# M = 25 ------------------------------------------------------------------
draws<-sample.dir(nn = 10, M = 25 , ln = 10)
xx<-c(seq(-4,4, length=10),5)
matplot(xx, draws, col=1:10, type="b")
curve(pnorm(x), add=T)
mean_vec = apply(draws,2,mean) #vector to hold values of mean for all the sets B1,B2,.....Bk, first element is the mean of observations
var_vec = apply(draws,2,var)
plot(mean_vec)
plot(var_vec)


# M = 50 ------------------------------------------------------------------

draws<-sample.dir(nn = 10, M = 50 , ln = 10)
xx<-c(seq(-4,4, length=10),5)
matplot(xx, draws, col=1:10, type="b")
curve(pnorm(x), add=T)
mean_vec = apply(draws,2,mean) #vector to hold values of mean for all the sets B1,B2,.....Bk, first element is the mean of observations
var_vec = apply(draws,2,var)
plot(mean_vec)
plot(var_vec)



# Construction of a CRP prior  --------------------------------------------

# define partion 
decide = function(table,prob_vector){
  return(sample(1:(length(table)+1),prob = prob_vector,size = 1,replace = T))    #0 existing table 1 new table
}

crp = function(num_customers,alpha){
  table = c(1)
  for(i in 2:num_customers){
    size.table = length(table)
    existing_table = table[1:length(table)]/(alpha + i)
    new_table = alpha/(alpha+i)
    prob = c(existing_table,new_table)
    val = decide(table,prob)
    if (val > length(table)){
      table[val] = 1
    }
    else{
      table[val] = table[val] + 1
    }
  }
  return(list(val,prob,table))
}


#for large values of alpha , the output is more dispersed that is more number of tables are formed 
#and customers sit in a disperesed state
rr = crp(200,25)
d = rr[[3]]
plot(1:length(d),d,type ='h',xlab = 'Cluster indicators',
     ylab = 'Number of people at the table', main =paste('CRP with alpha =',1))
#for low values of aplha output is not that dispersed, and many customers get concentrated on the 
#existing tables
rr = crp(200,1)
d = rr[[3]]
plot(1:length(d),d,type ='h',xlab = 'Cluster indicators',
     ylab = 'Number of people at the table', main =paste('CRP with alpha =',10))



# Algorithm 7 neal --------------------------------------------------

set.seed(1771650)
G0 = function(n){return(rnorm(n,125,75))} #random points on the real line

#generate data from gaussians centered far apart 
gen_yi = function(phi){
  
  vec1 = rnorm(80,phi[1])
#  vec2 = rnorm(20,phi[4])
  vec3 = rnorm(40,phi[8])
  vec4 = rnorm(40,phi[15])
  vec5 = rnorm(40,phi[20])
  yi = c(vec1,vec3,vec4,vec5)
  return(sample(yi))
}
 
#obtain indicators from crp 
get_ci = function(n_ci){
  vec = c()
  for (i in 1:length(n_ci)) {
    
    vec = c(vec,rep(i,n_ci[i]))
    
  }
  return(sample(vec))
}

#after getting indicators assign them parameters
table_params = function(c){
  size = length(c)
  m = 0
  val = c()
  for (i in c) {
    
    val[i] = rnorm(1,m,0)
    m = m + 10      #values distanced by 10 units  
  }
  return(val)
}

#function to sample indicators and table parameters according to Algorithm 7 Neal
myfunc = function(vec,y,phi_vect,M){
  
  phi_G0 = c()
  new_ind = c()
  for (i in 1:length(vec)) {
    
    fr = sum(vec[i]==vec)
    if (fr > 1) {                   #not a singleton
      phi.star = rnorm(1,mean = 125,sd = 100)
      nmr = dnorm(y[i],phi.star,1,log = T)
      dnr = dnorm(y[i],phi_vect[i],1,log = T)
      expr = (M/(length(y)-1))*exp(nmr-dnr)
      if(expr > runif(1)){    # acceptance probability 
        vec[i] = max(vec) + 1 #new table
        phi_vect[i] = phi.star
        new_ind = c(new_ind,vec[i]) #new table indicators
        phi_G0 = c(phi_G0,phi.star) #all phi drawn from G0 
      }
    }
    
    if (fr ==1) {                 # its a singleton 
      
      draw_from = sort(unique(vec[-i]))
      n_ic = table(vec[-i])       #frequency of clusters
      pr = n_ic/(length(y)-1)
      ci.star = sample(draw_from,1,prob = pr)
      idx = which(ci.star==vec)[1]
      phi.star = phi_vect[idx]
      nmr = dnorm(y[i],phi.star,1,log = T)
      dnr = dnorm(y[i],phi_vect[i],1,log = T)
      expr = ((length(y)-1)/M)*exp(nmr-dnr)
      if(expr > runif(1)){     #acceptance probability
        vec[i]=ci.star
        phi_vect[i]=phi.star
      }
    }
    
  }
  
  freq = table(vec)
  sing_idx = which(freq==1)
  ci = sort(unique(vec))
  idx = which(vec%in%ci[-sing_idx]) #all ids which are not singleton    
  for (i in idx) {
    n_ic = table(vec[-i])
    draw_from = sort(unique(vec[-i]))
    p = n_ic / (length(y)-1)
    #get corresponding phi_ci
    phi_ci = c()
    for (id in draw_from) {
      phi_id = which(id==vec)[1]
      phi_ci=c(phi_ci,phi_vect[phi_id])
    }
    p = p*dnorm(y[i],mean = phi_ci,1)
    dr = sample(draw_from,1,prob = p)
    vec[i] = dr
    phi_vect[i]=phi_ci[which(dr==draw_from)[1]]
    
  }
  #update phi or sample table parameters
  
  freq <- table(vec)
  vec.c <- sort(unique(vec))
  for(i in 1:length(vec.c)){
    idx <- which(vec==vec.c[i])
    y.tmp <- y[idx]
    post_u =  (sum(y.tmp)+(125/(100*2))) / ( (1/(100*2)) + length(y.tmp)) #updated posterior means and variance
    post_sd = 1 /((1/(100*2)) + lengths(y.tmp))
    #A <- length(y.tmp)/1
    #b <- sum(y.tmp)
    phi_vect[idx] <- rnorm(1,post_u,post_sd)
    
  }
  return(list(vec,phi_vect))
  
}



# function to initialize the states of the sampler
init_states = function(){
  
  M = 10    # concetration parameter
  num_obs = 200
  n_ci = crp(200,M)[[3]] #get indicators from crp
  c = 1:length(n_ci) 
  phi = table_params(c)   #assign table parameters to the indicators 
  ci = get_ci(n_ci)
  yi = gen_yi(phi)        #generate data
  
  
  return(list(phi,ci,yi,M))
  
}

#mcmc sampler
run_sampler = function(nsim = 1000){
  
  ##### init states ######
  lst = init_states()
  phi = lst[[1]]        #parameters
  vec = lst[[2]]        #indicators or ci
  yi = lst[[3]]         #data
  M = lst[[4]]          #alpha or DP parameter 
  phi_vect = phi[vec]
  
  #### init datastructures ##########
  ci_mat = matrix(NA, nrow = length(yi), ncol = nsim)   #holding datastructure for indicators
  ci_mat[,1] = vec
  phi_mat = matrix(NA, nrow = length(yi), ncol = nsim)  #holding datastructures for table parameters
  phi_mat[,1] = phi_vect
  
  #### run sampler ##################
  
  for (i in 2:nsim) {
    out = myfunc(vec,yi,phi_vect,M)    #function which samples indicators and table parameters
    vec = out[[1]]
    phi_vect = out[[2]]
    
    #table(vec)            #uncomment to view clusters
    #table(phi_vect)       #uncomment to view table parameters
    
    ci_mat[,i] = vec       #save the results
    phi_mat[,i] = phi_vect #save the results
    
  }
  return(list(phi[c(1,4,8,15,20)],yi,ci_mat,phi_mat))
  
}


##### run algorithm ########

res = run_sampler(5000)
orig_parameters = res[[1]] #original parameters from which data was simulated
orig_data = res[[2]]       # data 
mcmc_ci = res[[3]]         # markov chain for indicators
mcmc_phi = res[[4]]        # markov chain for parameters  
orig_parameters
orig_data
table(mcmc_ci[,500])
table(mcmc_phi[,100])

#### diagnostics ############

# acf plots
row = 10
par(mfrow = c(2,5))

### acf plot for number of distinct values at every iteration ####
k = c()
for (i in 1:ncol(mcmc_ci)){
  k[i]=length(unique(mcmc_ci[,i]))
}

#png(filename="mcmc_k.png")
plot(acf(k),main = paste('ACF plot for unique values k at every iteration'))
#dev.off()

#plot(autocorr(mcmc(k),lags = 1:500,relative = T))


# autocorrelation plot each theta_i from which data was sampled
# orig parameters 0,30,70,140,190
ids = c()
idx = which(mcmc_phi[,1]==0)[1]
ids[1]=idx
#png(filename="mcmc_theta0.png")
plot(acf(mcmc_phi[idx,][100:5000]),main = paste('Acf for mcmc chain of theta = 0'))
#dev.off()

idx = which(mcmc_phi[,1]==30)[1]
ids[2]=idx
#png(filename="mcmc_theta30.png")
plot(acf(mcmc_phi[idx,][100:5000]),main = paste('Acf for mcmc chain of theta = 30'))
#dev.off()

idx = which(mcmc_phi[,1]==70)[1]
ids[3]=idx
#png(filename="mcmc_theta70.png")
plot(acf(mcmc_phi[idx,][100:5000]),main = paste('Acf for mcmc chain of theta = 70'))
#dev.off()

idx = which(mcmc_phi[,1]==140)[1]
ids[4]=idx
#png(filename="mcmc_theta140.png")
plot(acf(mcmc_phi[idx,][100:5000]),main = paste('Acf for mcmc chain of theta = 140'))
#dev.off()

idx = which(mcmc_phi[,1]==190)[1]
ids[5]=idx
plot(acf(mcmc_phi[idx,][100:5000]),main = paste('Acf for mcmc chain of theta = 190'))


### distribution of parameters or thetas ######
par(mfrow = c(2,3))
for (i in ids){
  
  hist(mcmc_phi[i,][100:5000],col = 'orchid',probability = T)
  lines(density(mcmc_phi[i,][100:5000]),col = 'red', lwd = 4)
  
}




#### trace plots #####

# trace plot for number of distinct values at every iteration i
#png(filename="mcmc_k_trace.png")
plot(k,type = 'l',col ='black',main = paste('Trace plot for number of distinct values at each iteration'))
#dev.off()
# trace plots for sampled table parameters theta 

par(mfrow = c(2,3))
for (i in ids){
  plot(mcmc_phi[i,],type = 'l',col ='red')  
  title(main = paste('Trace plots for sampled table parameters'))
}


###### running mean plots for sampled table parameters ########
library(mcmcse)

par(mfrow = c(2,3))
for (i in ids){
  
estvssamp(mcmc(mcmc_phi[i,][100:5000]),col = 'red', lwd = 4, main = 'sample size vs mean estimate')  
}




### heidel.diag test for phi ######

for (i in ids){
  print('phi',str(i))  
  print(heidel.diag(mcmc(mcmc_phi[i,][100:1000]))) 
  
}

### heidel.diag test for indicators ci ######
for (i in ids){
  print('phi',str(i))  
  print(heidel.diag(mcmc(mcmc_ci[i,][100:1000]))) 
  
}

#### geweke test for phi #######
for (i in ids){
  
print(geweke.diag(mcmc(mcmc_phi[i,][100:5000])))
  
}





