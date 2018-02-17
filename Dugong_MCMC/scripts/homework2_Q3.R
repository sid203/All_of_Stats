
# Question 3 -------------------------------------------------------------------
#Plots
curve(dcauchy(x) * 1.5,lwd=3, col='red', from=-10, to = 10)
curve(dnorm(x),add=TRUE,col="black",lwd=3, from =-10, to = 10)
text(5,0.35,labels=expression(k~f[y](x)),col='red')
text(5,0.3,labels=expression(f[X](x)),col="black")
legend(x="topright",lty=1,lwd=1.4,col=c("red","black"),legend=c("Bounding","Target "))
title(main="A/R")

# Simulation --------------------------------------------------------------


ef=function(x){
  dnorm(x)
}

k=1.5

n_sim_aux=10000

Y=rep(NA,n_sim_aux)
E=rep(NA,n_sim_aux)
for(i in 1:n_sim_aux){
  Y[i]=rcauchy(1)
  E[i]=rbinom(1,size=1,prob=ef(Y[i])/k)
}
hist(Y,prob=TRUE,col = 'orchid')
hist(Y[E==1],prob=TRUE, col='orchid')
curve(dnorm(x),col="blue",lwd=2,add=TRUE)




# General Function --------------------------------------------------------



q=function(x){
  dcauchy(x)
}

draw_from_q=function(n){
  rcauchy(n)
}

f=function(x){
  dnorm(x)
}


AR=function(dtarget,dauxiliary,rauxiliary,k){
  
  count=0
  E=0
  
  while(E==0){
    candidate = rauxiliary(1)
    acc_prob=dtarget(candidate)/(k*dauxiliary(candidate))
    E = sample(c(1,0),prob=c(acc_prob, 1-acc_prob),size=1)
    count=count+1
  }
  
  return(list(draw=candidate,computational_effort=count))
  
}


AR(dtarget=q,dauxiliary=q,rauxiliary=draw_from_q,k)

mcsize=1000
draw_vec=rep(NA,mcsize)
effort_vec=rep(NA,mcsize)

for(i in 1:mcsize){
  
  DD=AR(dtarget=f,dauxiliary=q,rauxiliary=draw_from_q,k=3)
  draw_vec[i] = DD$draw
  effort_vec[i] = DD$computational_effort
  
}

hist(draw_vec,freq=FALSE)
curve(f(x),add=TRUE,lwd=3, col='red')
