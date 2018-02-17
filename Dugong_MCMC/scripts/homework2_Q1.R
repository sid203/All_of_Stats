set.seed(1221)
n_size=1
dugong = read.table('dugong-data.txt',header = TRUE)
Xi = dugong$Age
Yi = dugong$Length
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


# Generate Yi for every value of mu and tau-------------------------------------------------------------

gen_Yi = function(mu_samples,tau,size) { 
  Yi_samples = matrix(nrow = size, ncol = length(mu_samples))
  for (i in 1:length(mu_samples)){
    Yi_samples[,i] = rnorm(size,mu_samples[i],tau)
  }
  return(Yi_samples)
  
}

y = gen_Yi(mu_samples,tau,size = 100)
# I am not using these y variable anywhere


# Negative log likelihood -------------------------------------------------


fn = function(vec,y_vec,x_i){
  #sample size depends on length of alpha
  alpha_vec=vec[1]
  beta_vec = vec[2]
  gam_vec = vec[3]
  tau=vec[4]
  Xi = x_i
  mu = alpha_vec-(beta_vec*((gam_vec)^Xi))
  term_1 = sum((y_vec-mu)^2)/(tau^2)
  term_2 = -term_1 * (0.5)
  const = -0.5*length(y_vec)*log(2*pi*(tau^2))
  expres = -(const + term_2)
  return(expres)
}


#MLE estimates of all samples of Yi
# I just guessed that the original values might be a good starting point for optimization
o=optim(c(alpha,beta,gam,tau),fn,gr = NULL,Yi,Xi,hessian = TRUE)
alpha.cap = o$par[1]
beta.cap=o$par[2]
gam.cap=o$par[3]
tau.cap=o$par[4]


estimates = data.frame(alpha.cap,beta.cap,gam.cap,tau.cap) #dataframe of MLE Estimates

# MAP ---------------------------------------------------------------------

fn_log_likelihood_prior = function(vec,y_vec,x_i){
  #Likelihood function
  alpha_vec=vec[1]
  beta_vec = vec[2]
  gam_vec = vec[3]
  tau=vec[4]
  Xi = x_i
  mu = alpha_vec-(beta_vec*((gam_vec)^Xi))
  term_1 = sum((y_vec-mu)^2)/(tau^2)
  term_2 = -term_1 * (0.5)
  const = -0.5*length(y_vec)*log(2*pi*(tau^2))
  expres = -(const + term_2)
  #Prior distro p(alpha)*p(beta)*p(gamma)*p(tau)
  val = -log( dnorm(alpha_vec,0,5)*dnorm(beta_vec,0,2)*dunif(gam_vec,0,1)*(1/dgamma(tau,2,3))) 
  return(expres+val)
}



# I just guessed that the original values might be a good starting point for optimization
o_map=optim(c(alpha,beta,gam,tau),fn_log_likelihood_prior,gr = NULL,Yi,Xi,hessian = TRUE)
alpha.cap_map = o_map$par[1]
beta.cap_map=o_map$par[2]
gam.cap_map=o_map$par[3]
tau.cap_map=o_map$par[4]

estimates_map = data.frame(alpha.cap_map,beta.cap_map,gam.cap_map,
                           tau.cap_map) #dataframe of MAP Estimates

print(estimates) #MLE ESTIMATES FOR alpha,beta,gamma,tau
print(estimates_map)   #MAP estimate for alpha,beta,gamma,tau
