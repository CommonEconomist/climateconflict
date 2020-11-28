#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Regression models
# See also:
# https://sourceforge.net/p/mcmc-jags/discussion/610036/thread/7d4aa94b/
# Last update 2017 06 15
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Libraries
library(coda)
library(R2jags)
library(rjags)
library(jagstools)

# 0) Baseline model 
# - Only conflict dynamics
M0<-function (){
  for(i in 1:N){
    mu[i]<-a0+b1*conflict.l[i]+b2*conflict.w[i]
    lambda[i]<-exp(mu[i])
    p[i]<-r/(r+lambda[i])
    conflict[i]~dnegbin(p[i],r)     
    fitted[i]<-mu[i] # Predicted values
    
    }  
  
  # Priors
  a0~dnorm(0,.01)   # Intercept
  b1~dnorm(0,.01)   # Temporal lag
  b2~dnorm(0,.01)   # Spatial lag
  r~dgamma(.01,.01) # Gamma distribution 
}

# 1) General model
# - Pooled model
# - Including conflict dynamics
M1<-function (){
  for(i in 1:N){
    mu[i]<-a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]
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
  r~dgamma(.01,.01) # Gamma distribution 
}

# 2) Additional controls 
# - Pooled model
# - Including conflict dynamics
# - Variables on population density and livelihood zone
M2<-function (){
  for(i in 1:N){
    mu[i]<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]+
      b4*pop[i]+b5*lz[i]
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
  r~dgamma(.01,.01) # Gamma distribution 
}

# 3) Interaction effects 
# - Pooled model
# - Including conflict dynamics
# - Variables on population density and livelihood zone
M3<-function (){
  for(i in 1:N){
    mu[i]<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]+
      b4*int[i]+b5*rain[i]*int[i]
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
  b4~dnorm(0,.01)   # Interaction variable
  b5~dnorm(0,.01)   # Interaction effect
  r~dgamma(.01,.01) # Gamma distribution 
}

# 4) Model for cross-validation 
M2a<-function (){
  for(i in 1:N){
    mu[i]<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b4*pop[i]+b5*lz[i]
    lambda[i]<-exp(mu[i])
    p[i]<-r/(r+lambda[i])
    conflict[i]~dnegbin(p[i],r)     
    fitted[i]<-mu[i] # Predicted values
    }  
  
  # Priors
  a0~dnorm(0,.01)   # Intercept
  b1~dnorm(0,.01)   # Temporal lag
  b2~dnorm(0,.01)   # Spatial lag
  b4~dnorm(0,.01)   # Population density
  b5~dnorm(0,.01)   # Livelihood zone
  r~dgamma(.01,.01) # Gamma distribution 
}

# 5) Model for certain spatial dynamics 
M4<-function (){
  for(i in 1:N){
    mu[i]<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]+
      b4*pop[i]+b5*lz[i] + b6*W.rain[i]
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
  b6~dnorm(0,.01)   # Change in rain neighbouring districts
  r~dgamma(.01,.01) # Gamma distribution 
}

# 5) Model with Kenya indicator
M5<-function (){
  for(i in 1:N){
    mu[i]<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]+
      b4*pop[i]+b5*lz[i] + b6*kenya[i]
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
  b6~dnorm(0,.01)   # Kenya-1
  r~dgamma(.01,.01) # Gamma distribution 
}

# 6) Additional controls 
# - Pooled model
# - Including conflict dynamics
# - Variables on population density and livelihood zone
M2i<-function (){
  for(i in 1:N){
    mu[i]<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]+
      b4*pop[i]+b5*lz[i] +b6*kenya[i]
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
  b6~dnorm(0,.01)   # Livelihood zone
  r~dgamma(.01,.01) # Gamma distribution 
}

# 7) Survival model: Baseline hazard 
# Duration effects fitted as a quadratic
S1<-function () {
  for(i in 1:N) {
    incidence[i]~dbern(yhat[i])
    logit(yhat[i])<-a0 + a1*t[i] + a2*t[i]^2
    fit[i]<-yhat[i] # Predicted values
  } 
    
  # Priors
  a0~dnorm(0,.01)
  a1~dnorm(0,.01)
  a2~dnorm(0,.01)
}

# 8) Including precipitation 
S2<-function () {
  for(i in 1:N) {
    incidence[i]~dbern(yhat[i])
    logit(yhat[i])<-a0 + a1*t[i] + a2*t[i]^2 + b1*episode[i] + b2*rain[i] 
    fit[i]<-yhat[i] # Predicted values
  } 
    
  # Priors
  a0~dnorm(0,.01)
  a1~dnorm(0,.01)
  a2~dnorm(0,.01)
  b1~dnorm(0,.01)
  b2~dnorm(0,.01)
}

# 9) Unobserved heterogeneity 
S3<-function () {
  for(i in 1:N) {
    incidence[i]~dbern(yhat[i])
    logit(yhat[i])<-u[D[i]] + a1*t[i] + a2*t[i]^2 + b1*episode[i] + b2*rain[i] + b3*W[i]
    fit[i]<-yhat[i] # Predicted values
  } 
    
  for(j in 1:J){
    u[j]~dnorm(mu_u[j],sd_u^-2)
    mu_u[j]<-u0
  }
  # Priors
  u0~dnorm(0,.01)
  a1~dnorm(0,.01)
  a2~dnorm(0,.01)
  b1~dnorm(0,.01)
  b2~dnorm(0,.01)
  b3~dnorm(0,.01)
  sd_u~dunif(0,100)
}

# 10) Interaction 
S4<-function () {
  for(i in 1:N) {
    incidence[i]~dbern(yhat[i])
    logit(yhat[i])<-u[D[i]] + a1*t[i] + a2*t[i]^2 +a3*period[i] + 
      b1*episode[i] + b2*rain[i] + b3*W[i] + 
      c1*period[i]*rain[i]
    fit[i]<-yhat[i] # Predicted values
  } 
    
  for(j in 1:J){
    u[j]~dnorm(mu_u[j],sd_u^-2)
    mu_u[j]<-u0
  }
  # Priors
  u0~dnorm(0,.01)
  a1~dnorm(0,.01)
  a2~dnorm(0,.01)
  b1~dnorm(0,.01)
  b2~dnorm(0,.01)
  b3~dnorm(0,.01)
  c1~dnorm(0,.01)
  a3~dnorm(0,.01)
  sd_u~dunif(0,100)
}

## FIN