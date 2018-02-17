

# Create_Stationary_Distribution ------------------------------------------
S=c(1,2,3)        # discrete state space
tran_matrix = matrix(c(0,1/2,1/2,5/8,1/8,1/4,2/3,1/3,0),nrow = 3, ncol = 3,byrow = T)
tran_matrix
eigen(t(tran_matrix))$vector[,1]
pi <- eigen(t(tran_matrix))$vector[,1]/sum(eigen(t(tran_matrix))$vector[,1])
# Simulation Part a--------------------------------------------------------------

x1<-1             ## starting value of the chain
nsample<-1000
chain<-rep(NA,nsample+1) # vector that will hold
chain[1]<-x1 
for(t in 1:nsample){
  chain[t+1]<-sample(S,size=1,prob=tran_matrix[chain[t],])
}
plot(chain,ylim=c(0,4),type="l")
plot(chain,ylim=c(0,4))


# Part b ------------------------------------------------------------------
result = matrix(nrow = 3, ncol = 3)
result[,1] = c(table(chain)[1]/nsample,table(chain)[2]/nsample,table(chain)[3]/nsample)
# Part C ------------------------------------------------------------------
x1<-1             ## starting value of the chain
nsample<-1000
nmarkov = 500
chain<-rep(NA,nsample+1) # vector that will hold
final_states = rep(NA,nmarkov)
chain[1]<-x1 

for (i in 1:nmarkov)
{
  for(t in 1:nsample){
    chain[t+1]<-sample(S,size=1,prob=tran_matrix[chain[t],])
    
  }
  final_states[i] = chain[length(chain)]
}

plot(final_states,ylim=c(0,4),col='blue')
plot(final_states,ylim=c(0,4),col='purple')
print ('Relative Frequency of Final States')
result[,2] = c(table(final_states)[1]/nmarkov,table(final_states)[2]/nmarkov,table(final_states)[3]/nmarkov)

#Because the transition matrix remains the same everytime we simulate the chain, so we target the same distribtuion as we do it in part a.
#Also, by recording every 500th value each time we simulate the chain would mean we are thinning the chain. If we had simulated the chain 
#in part a for 500*1000 times and recorded every 500th value in it, it would mean the same thing. But of course we dont specify the starting 
#space thru this way as we always specify the starting state in this part c. But again, the behavior of the chain is not affected by the starting 
#state in this setup of discrete space. 
# Part d ------------------------------------------------------------------

S=c(1,2,3)        # discrete state space
tran_matrix = matrix(c(0,1/2,1/2,5/8,1/8,1/4,2/3,1/3,0),nrow = 3, ncol = 3,byrow = T)
pi <- eigen(t(tran_matrix))$vector[,1]/sum(eigen(t(tran_matrix))$vector[,1])
# Part e ------------------------------------------------------------------


result[,3] = pi
result = as.data.frame(result)
`colnames<-`(result,c('Relative Frequency','Final State Frequency','Stationary'))
#The distribution is well approximated

# Part f ------------------------------------------------------------------
x1<-1             ## starting value of the chain
nsample<-1000
chain<-rep(NA,nsample+1) # vector that will hold
chain[1]<-x1 
for(t in 1:nsample){
  chain[t+1]<-sample(S,size=1,prob=tran_matrix[chain[t],])
}
plot(chain,ylim=c(0,4),col='blue')
plot(chain,ylim=c(0,4),type="b",col='purple')
result = matrix(nrow = 3, ncol = 3)
result[,1] = c(table(chain)[1]/nsample,table(chain)[2]/nsample,table(chain)[3]/nsample)
x1<-1             ## starting value of the chain
nsample<-1000
nmarkov = 500
chain<-rep(NA,nsample+1) # vector that will hold
final_states = rep(NA,nmarkov)
chain[1]<-x1 

for (i in 1:nmarkov)
{
  for(t in 1:nsample){
    chain[t+1]<-sample(S,size=1,prob=tran_matrix[chain[t],])
    
  }
  final_states[i] = chain[length(chain)]
}
plot(final_states,ylim=c(0,4),col='blue')
plot(final_states,ylim=c(0,4),col='purple')
print ('Relative Frequency of Final States')
result[,2] = c(table(final_states)[1]/nmarkov,table(final_states)[2]/nmarkov,table(final_states)[3]/nmarkov)
S=c(1,2,3)        # discrete state space
tran_matrix = matrix(c(0,1/2,1/2,5/8,1/8,1/4,2/3,1/3,0),nrow = 3, ncol = 3,byrow = T)
pi <- eigen(t(tran_matrix))$vector[,1]/sum(eigen(t(tran_matrix))$vector[,1])


result[,3] = pi
result = as.data.frame(result)
`colnames<-`(result,c('Relative Frequency','Final State Frequency','Stationary'))
