# Q2 e part ---------------------------------------------------------------



#Consider likelihood as Bernoulli with n=5 observations and y1+y2...y5=1
#Prior as dbeta(3,2)
#So posterior is dbeta(3+1,2+5-1) = dbeta(4,6)

q=function(x){
  dbeta(x,3,2)
}

draw_from_q=function(n){
  rbeta(n,3,2)
}

f=function(x){
  dbeta(x,4,6)
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
  
  DD=AR(dtarget=f,dauxiliary=q,rauxiliary=draw_from_q,k=4)
  draw_vec[i] = DD$draw
  effort_vec[i] = DD$computational_effort
  
}

hist(draw_vec,freq=FALSE)
curve(f(x),add=TRUE,lwd=3, col='red')

