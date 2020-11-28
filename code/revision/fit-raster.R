#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Fit raster models
# Last update 2018 03 22
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# 1) Regression models
set.seed(42);m1<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,N=N),
                      parameters.to.save=c("a0","b1","b2","b3"),
                      model.file=M1,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

set.seed(42);m2<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                pop=X$afuds95ag,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

set.seed(42);m7<-jags(data=list(conflict=X$nkill.com,conflict.l=X$nkill.com.l,
                                conflict.w=X$W.nkill.com,rain=X$belg.g,
                                pop=X$afuds95ag,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)


# 2) Cross-validation
# Vectors and lists
X<-X[order(X$cell_number),]

S<-as.numeric(factor(X$cell_number))
J<-max(S) 
Y<-X$events.com

yhat<-list()
beta<-list()

# Fit model: leave-one-out
for(i in 1:J){
  print(i)
  observed=X$events.com
  observed[S==i]<-NA # Set observed values to NA for state
  
  # Fit model
  set.seed(42);M<-jags(data=list(conflict=observed,conflict.l=X$events.com.l,
                                 conflict.w=X$W.events.com,rain=X$belg.g,
                                 pop=X$afuds95ag,lz=X$pastoral,N=N),
                       parameters.to.save=c("b3","fitted"),
                       model.file=M2,n.chains=3,n.iter=1000,
                       n.burnin=400,n.thin=2)
  
  # Estimated parameters to matrix
  yhatM<-as.matrix(M$BUGSoutput$sims.list$fitted) 
  betaM<-as.matrix(M$BUGSoutput$sims.list$b3)     
  
  # Results for i
  yhat[[i]]<-yhatM[,i] # Fitted values to vector
  beta[[i]]<-betaM[,1] # Estimated beta to vector
}

# Save results
save.image(file='output/raster_results.RData')

## FIN