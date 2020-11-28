#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Accounting for temperature
# Request by R2
# Last update 2018 05 08
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source("code/load.R")
# Functions 
stan<-function(x){(x-mean(x,na.rm=TRUE))/(2*sd(x,na.rm=TRUE))}

# 1) Prepare data
load('data/temperature.RData')
temp$temp.d<-stan(temp$temp.d)
X<-merge(X,temp[,c('row_name','temp.d')])

# 2) Specify model
M<-function (){
  for(i in 1:N){
    mu[i]<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]+ b4*pop[i]+b5*lz[i]
    +b6*temp[i]
    
    lambda[i]<-exp(mu[i])
    p[i]<-r/(r+lambda[i])
    conflict[i]~dnegbin(p[i],r)     
    fitted[i]<-mu[i] # Predicted values
  }  
  
  # Priors
  a0~dnorm(0,.01)   # Intercept
  b1~dnorm(0,.01)   # Temporal lag
  b2~dnorm(0,.01)   # Spatial lag
  b3~dnorm(0,.01)   # Rainfall
  b4~dnorm(0,.01)   # Population density
  b5~dnorm(0,.01)   # Livelihood zone
  b6~dnorm(0,.01)   # Temperature
  r~dgamma(.01,.01) # Gamma distribution 
}

# 3) Estimate model
set.seed(42);m<-jags(data=list(conflict=X$events.com,conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                pop=X$p00.km2,lz=X$pastoral,temp=X$temp.d,
                                N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5","b6",
                                           "fitted"),
                      model.file=M,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Results
print(m$BUGSoutput$summary[1:8,c(1,2,3,7)],digits.summary=3) # Col. 4

save.image('output/including_temperature.RData')

## FIn