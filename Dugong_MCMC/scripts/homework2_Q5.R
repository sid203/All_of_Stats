dugong = read.table('dugong-data.txt',header = TRUE)
Xi = dugong$Age
Yi = dugong$Length
ny = length(Yi)
nsims = 10000
set.seed(1221)
n_size=1
#conditional distros
# Alpha = Normal(alpha_sd^2 * sum(Yi+Beta*Gamma^Xi)/alpha_sd²*n+tau² , tau²*alpha_sd²/alpha_sd²*n+tau²)



# Generate alpha  ---------------------------------------------------------
alpha_sd=5
a=1
b = Inf
pa=pnorm(a,0,alpha_sd)
pb=pnorm(b,0,alpha_sd)
alpha=alpha_sd*qnorm(runif(n_size,pa,pb))

# Generate beta -----------------------------------------------------------
beta_sd=2
pa_b=pnorm(a,0,beta_sd)
pb_b=pnorm(b,0,beta_sd)
beta=beta_sd*qnorm(runif(n_size,pa_b,pb_b))

# Generate gamma ----------------------------------------------------------
gam=runif(n_size,0,1)

# Generate Tau ------------------------------------------------------------

tau=1/rgamma(n_size,2,3)


# Generate mu -------------------------------------------------------------
gen_mu_samples = function(alpha,beta,gam,Xi) 
{  return(alpha-(beta*((gam)^Xi)))
  
}

mu_samples = gen_mu_samples(alpha,beta,gam,Xi)


# Initialize parameters  --------------------------------------------------
alpha_pos = array(alpha,dim = nsims)
beta_pos = array(beta, dim = nsims)
gamma_pos = array(gam,dim = nsims)
tau_pos = array(tau,dim = nsims)

#Implement Random Walk for Gamma distro
#and obtain the samples for conditional of gamma
# Algorithm Starts --------------------------------------------------------
log_likelihood = function(y_vec,x_vec,alp,bet,gam,tau)
{
  total = 0
  for (i in 1:length(y_vec))
  {
    temp = gam ^ x_vec[i]
    temp = bet * temp
    temp = alp - temp
    temp = temp **2
    temp = y_vec[i] - temp
    total= total+temp
  }
  total / -(2 * (tau^2))
}

# run the algorithm -------------------------------------------------------
n = 10000
current_gam = 0.634 
samps = rep(NA,n)
for (i in 1:n)
{
  
  proposed = rnorm(1, current_gam, 0.4) 
  while(proposed<0 | proposed>1)
  {
    proposed = rnorm(1, current_gam, 0.4) 
  }
  logr = log_likelihood(Yi,Xi,alpha,beta,proposed,tau)-log_likelihood(Yi,Xi,alpha,beta,current_gam,tau)
  if (log(runif(1))< logr) {current_gam = proposed}
  
  samps[i] = current_gam
}
length(unique(samps))/n
hist(samps,col = 'orchid')
length(samps)
plot(samps[100:10000],type = 'l',lwd=1)
mean(samps[500:10000])
acf(samps)

gamma_pos=samps


# Gibbs Sampling section --------------------------------------------------


for (i in 2:nsims){
  # Parameters for conditional of alpha  ------------------------------------
  
  alpha_var=alpha_sd^2
  alpha_val=sum(Yi+beta_pos[i-1]*(gamma_pos[i-1]^Xi))
  alpha_pos_mean=(alpha_var*alpha_val)/(alpha_var*ny+tau_pos[i-1])
  alpha_pos_std=tau_pos[i-1]*alpha_var/(alpha_var*ny+tau_pos[i-1])
  k=rnorm(1,alpha_pos_mean,alpha_pos_std)
  alpha_pos[i] = k 
  
  #  Parameters for conditional of beta -------------------------------------
  beta_var = beta_sd^2
  beta_val = sum((alpha_pos[i]-Yi)*(gamma_pos[i-1]^Xi))
  beta_pos_mean = beta_var*beta_val/(beta_var+tau_pos[i-1])
  beta_pos_std = tau_pos[i-1]*beta_var/(beta_var+tau_pos[i-1])
  k_b=rtruncnorm(1,1,Inf,beta_pos_mean,beta_pos_std)
  beta_pos[i] = k_b 
  
  
  
  
  # Parameters for conditional of tau ---------------------------------------
  
  tau_pos_shape = (2+ny)
  tau_pos_rate = (6+sum((Yi-(alpha_pos[i]-beta_pos[i]*(gamma_pos[i-1]^Xi)))^2))/2
  tau_pos[i] = 1/rgamma(1,tau_pos_shape,tau_pos_rate)

  
  
}



# Graphs Question 5 part d ----------------------------------------------------

# For alpha ---------------------------------------------------------------

par(mfrow=c(4,1))

plot(alpha_pos[1:100],type="l",main=paste("First 100 iterations"))
title(sub=paste("Mean estimate =",round(mean(alpha_pos),4)," - RAW Variance estimate =",round(var(alpha_pos),4)))
plot(alpha_pos[500:length(alpha_pos)],type="l",main="Removed/burnt the first 500 simulations")
title(sub=paste("Mean estimate =",round(mean(alpha_pos[501:length(alpha_pos)]),4)," - RAW Variance estimate =",round(var(alpha_pos[501:length(alpha_pos)]),4)))

hist(alpha_pos[500:length(alpha_pos)],freq=F)
lines(density(alpha_pos[500:length(alpha_pos)]))
acf(alpha_pos)

# For Beta ----------------------------------------------------------------
par(mfrow=c(4,1))

plot(beta_pos[1:100],type="l",main=paste("First 100 iterations"))
title(sub=paste("Mean estimate =",round(mean(beta_pos),4)," - RAW Variance estimate =",round(var(beta_pos),4)))
plot(alpha_pos[500:length(beta_pos)],type="l",main="Removed/burnt the first 500 simulations")
title(sub=paste("Mean estimate =",round(mean(beta_pos[501:length(beta_pos)]),4)," - RAW Variance estimate =",round(var(beta_pos[501:length(beta_pos)]),4)))

hist(beta_pos[500:length(beta_pos)],freq=F)
lines(density(beta_pos[500:length(beta_pos)]))
acf(beta_pos)


# For Gamma ---------------------------------------------------------------
par(mfrow=c(4,1))

plot(gamma_pos[1:100],type="l",main=paste("First 100 iterations"))
title(sub=paste("Mean estimate =",round(mean(gamma_pos),4)," - RAW Variance estimate =",round(var(gamma_pos),4)))
plot(gamma_pos[500:length(gamma_pos)],type="l",main="Removed/burnt the first 500 simulations")
title(sub=paste("Mean estimate =",round(mean(gamma_pos[501:length(gamma_pos)]),4)," - RAW Variance estimate =",round(var(gamma_pos[501:length(gamma_pos)]),4)))

hist(gamma_pos[500:length(gamma_pos)],freq=F)
lines(density(gamma_pos[500:length(gamma_pos)]))
acf(gamma_pos)



# For Tau -----------------------------------------------------------------
par(mfrow=c(4,1))

plot(tau_pos[1:100],type="l",main=paste("First 100 iterations"))
title(sub=paste("Mean estimate =",round(mean(tau_pos),4)," - RAW Variance estimate =",round(var(tau_pos),4)))
plot(tau_pos[500:length(tau_pos)],type="l",main="Removed/burnt the first 500 simulations")
title(sub=paste("Mean estimate =",round(mean(tau_pos[501:length(tau_pos)]),4)," - RAW Variance estimate =",round(var(tau_pos[501:length(tau_pos)]),4)))

hist(tau_pos[500:length(tau_pos)],freq=F)
lines(density(tau_pos[500:length(tau_pos)]))
acf(tau_pos)






# question 5 part e -------------------------------------------------------


par(mfrow=c(1,1))
# graphical behaviour for alpha with growing t ----------------------------
sum_vector=cumsum(alpha_pos)
v = rep(NA,length(alpha_pos))
for (i in 1:length(v))
{
  v[i] = sum_vector[i] / i
}
length_vector = seq(1,nsims)
plot(length_vector,v,type = 'p',xlab = 't = 1....T',ylab='empirical_mean of alpha')




# graphical behaviour of beta with growing t ------------------------------
sum_vector=cumsum(beta_pos)
v = rep(NA,length(beta_pos))
for (i in 1:length(v))
{
  v[i] = sum_vector[i] / i
}
length_vector = seq(1,nsims)
plot(length_vector,v,type = 'p',xlab = 't = 1....T',ylab='empirical_mean for beta')



# graphical behaviour of gamma with growing t -----------------------------
sum_vector=cumsum(gamma_pos)
v = rep(NA,length(gamma_pos))
for (i in 1:length(v))
{
  v[i] = sum_vector[i] / i
}
length_vector = seq(1,nsims)
plot(length_vector,v,type = 'p',xlab = 't = 1....T',ylab='empirical_mean for gamma')





# graphical behaviour of tau with growing t -------------------------------
sum_vector=cumsum(tau_pos)
v = rep(NA,length(tau_pos))
for (i in 1:length(v))
{
  v[i] = sum_vector[i] / i
}
length_vector = seq(1,nsims)
plot(length_vector,v,type = 'p',xlab = 't = 1....T',ylab='empirical_mean for tau')





# Question 5 part f -------------------------------------------------------


# estimates for alpha and its Error --------------------------------------------
batch_function = function(chain,bins){
  overall_mean = mean(chain)
  result = split(chain, cut(seq(chain), bins, labels = FALSE))
  
}

batch_size = nsims / 10
mean_array = rep(NA,10)
alpha_error = 0
index = 1
for (i in 1:10){
  current_batch = alpha_pos[index:(i * batch_size) ]
  mean_array[i] = mean(current_batch)
  mu_i = mean(current_batch) - mean(alpha_pos)
  mu_i = mu_i **2
  alpha_error=alpha_error+mu_i
  index = index+batch_size
}
mean(mean_array)
alpha_error # estimate for variance of alpha
#confidence interval 
upper_val = mean(alpha_pos)+qnorm(0.95)*sqrt(alpha_error)/sqrt(nsims)
lower_val = mean(alpha_pos)-qnorm(0.95)*sqrt(alpha_error)/sqrt(nsims)


# estimates for beta and its error ----------------------------------------
batch_size = nsims / 10
mean_array = rep(NA,10)
beta_error = 0
index = 1
for (i in 1:10){
  current_batch = beta_pos[index:(i * batch_size) ]
  mean_array[i] = mean(current_batch)
  mu_i = mean(current_batch) - mean(beta_pos)
  mu_i = mu_i **2
  beta_error=beta_error+mu_i
  index = index+batch_size
}
mean(mean_array)
beta_error# estimate for variance of beta
#confidence interval 
upper_val = mean(beta_pos)+qnorm(0.95)*sqrt(beta_error)/sqrt(nsims)
lower_val = mean(beta_pos)-qnorm(0.95)*sqrt(beta_error)/sqrt(nsims)



# estimates for gamma and its error ---------------------------------------
batch_size = nsims / 10
mean_array = rep(NA,10)
gamma_error = 0
index = 1
for (i in 1:10){
  current_batch = gamma_pos[index:(i * batch_size) ]
  mean_array[i] = mean(current_batch)
  mu_i = mean(current_batch) - mean(gamma_pos)
  mu_i = mu_i **2
  gamma_error=gamma_error+mu_i
  index = index+batch_size
}
mean(mean_array)
gamma_error # estimate for variance of gamma
#confidence interval 
upper_val = mean(gamma_pos)+qnorm(0.95)*sqrt(gamma_error)/sqrt(nsims)
lower_val = mean(gamma_pos)-qnorm(0.95)*sqrt(gamma_error)/sqrt(nsims)



# Estimate for tau --------------------------------------------------------

batch_size = nsims / 10
mean_array = rep(NA,10)
tau_error = 0
index = 1
for (i in 1:10){
  current_batch = tau_pos[index:(i * batch_size) ]
  mean_array[i] = mean(current_batch)
  mu_i = mean(current_batch) - mean(tau_pos)
  mu_i = mu_i **2
  tau_error=tau_error+mu_i
  index = index+batch_size
}
mean(mean_array)
tau_error # estimate for variance of alpha
#confidence interval 
upper_val = mean(tau_pos)+qnorm(0.95)*sqrt(tau_error)/sqrt(nsims)
lower_val = mean(tau_pos)-qnorm(0.95)*sqrt(tau_error)/sqrt(nsims)



# Question 5 part g -------------------------------------------------------

#The parameter having largest variance will be the one with highest posterior uncertainty
# And the parameter gamma has largest variance among the 4 parameters because large variance results
#in a larger confidence interval which is a measure of increased uncertainty
gamma_error

# Question 5 part h -------------------------------------------------------

cor(alpha_pos[500:10000],beta_pos[500:10000]) #0.6566756
cor(alpha_pos[500:10000],gamma_pos[500:10000]) #0.5880584
cor(alpha_pos[500:10000],tau_pos[500:10000]) #-0.1234351
cor(beta_pos[500:10000],gamma_pos[500:10000]) #0.2274154
cor(beta_pos[500:10000],tau_pos[500:10000]) #0.04435232
cor(gamma_pos[500:10000],tau_pos[500:10000]) #-0.1478579

#alpha and beta have the largest correlation

# Question 5 part i  ------------------------------------------------------

#Age = 20
#Predictive density 
par(mfrow = c(2,1))
x_new = 20
#For all the simulated samples i create a vector where each entry is ui=alphai-betai*(gamma^x_new) , for one single value of alpha,beta,and gamma

updated_mean = alpha_pos-beta_pos*(gamma_pos^x_new)
updated_tau = tau_pos
draw_sample_20 = 0

for (i in 1:length(updated_mean)){
    draw_sample_20 = draw_sample_20+ rnorm(100,updated_mean[i],updated_tau[i])
}



#Posterior predictive denisty samples 
post_pred_samp = draw_sample_20/nsims
hist(post_pred_samp)

# Question 5 part j -------------------------------------------------------


#For age 30 
x_30 = 30
updated_mean_30 = alpha_pos-beta_pos*(gamma_pos^x_30)
updated_tau_30 = tau_pos
num = 1000
draw_sample = 0
for (i in 1:length(updated_mean_30)){
    draw_sample = draw_sample+ rnorm(100,updated_mean_30[i],updated_tau_30[i])
    
  }


post_pred_samp_30 = draw_sample/nsims
hist(post_pred_samp_30)


# Question 5 part k -------------------------------------------------------

v_1=var(post_pred_samp)
v_2=var(post_pred_samp_30)

# Since v_1 is less than v_2 , the prediction of Xi = 20 is more precise 