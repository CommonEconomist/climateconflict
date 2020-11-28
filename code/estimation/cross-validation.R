#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Cross-validation 
# Omitting one district at a time
# Last update 2018 05 14
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source("code/load.R")

# 1) Prepare data 
X<-X[order(X$row_name),]

# Indices
S<-as.numeric(factor(X$row_name))
J<-max(S) 
Y<-X$events.com

# Vectors
yhat0<-list()
yhat<-list()
beta<-list()

# 2.1) Fit main model
for(i in 1:J){
  print(i)
  observed=X$events.com
  observed[S==i]<-NA # Set observed values to NA for state
  
  # Fit model
  set.seed(42);M<-jags(data=list(conflict=observed,conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("b3","fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)
  
  # Estimated parameters to matrix
  yhatM<-as.matrix(M$BUGSoutput$sims.list$fitted) 
  betaM<-as.matrix(M$BUGSoutput$sims.list$b3)     
  
  # Results for i
  yhat[[i]]<-yhatM[,i] # Fitted values to vector
  beta[[i]]<-betaM[,1] # Estimated beta to vector
}
 

# 2.2) Without rainfall variable
for(i in 1:J){
  print(i)
  observed=X$events.com
  observed[S==i]<-NA # Set observed values to NA for state
  
  # Fit model
  set.seed(42);M<-jags(data=list(conflict=observed,conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save="fitted",
                      model.file=M2a,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)
  
  yhatM<-as.matrix(M$BUGSoutput$sims.list$fitted) # Fitted values to matrix
  yhat0[[i]]<-yhatM[,i]                           # Fitted values to vector
  
}

# 3) Calculate error measures 
error<-c()
error0<-c()
error.d<-list()
for(i in 1:J){
  error[i]=exp(mean(yhat[[i]]))-Y[i]
  error0[i]=exp(mean(yhat0[[i]]))-Y[i]
  error.d[[i]]=exp(yhat[[i]])-Y[i]
}

## Save
save.image(file="output/cross_validation.RData")

## FIN