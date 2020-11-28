#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Cross-section time-series regression
# Request by R2
# Creates figure 9
# Last update 2018 05 14
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Functions
stan<-function(x){(x-mean(x,na.rm=TRUE))/(2*sd(x,na.rm=TRUE))}

# Libraries
library(DataCombine)
library(spdep)
library(coda)
library(R2jags)
library(rjags)
library(jagstools)

# 1) Prepare data
load("data/CenTrend.RData")          # Precipitation
load("data/communal_violence.RData") # Communal violence
load("raw_data/gaulk_adm2.RData")    # District boundaries
load("data/lzones.RData")            # Livelihood zones
load("data/pop_dens.RData")          # Population density

# Subset year > 1989 and merge 
dat<-dat[dat$year>=1989,]
X<-merge(dat[,c("row_name","year","belg.ma")],
         communal[,c("row_name","year","communal")],all.x=TRUE)
X$communal[is.na(X$communal)]<-0 # Set NA to zero

# Calculate temporal lag
X<-X[order(X$row_name,X$year),]
X<-slide(X,Var="communal",GroupVar="row_name",NewVar="communal.l",slideBy=-1)

# Load district boundaries
gaul_k<-gaul_k[gaul_k$row_name %in% X$row_name,]

# Adjacency matrix
k<-poly2nb(gaul_k,queen=TRUE) # Takes some seconds
W<-nb2listw(k,style="B",zero.policy=TRUE)
adj<-as.matrix(listw2mat(W))
ident<-diag(1,nrow=length(unique(X$year)))

# Calculate lag
X<-X[order(X$year,X$row_name),]
X$W.communal<-as.numeric((ident%x%adj)%*%X$communal) 

# Add other data
X<-merge(X,pop.dens[,c("row_name","p00.km2")],all.x=TRUE)
lz<-data.frame(row_name=adm2$row_name,pastoral=adm2$pastoral)
X<-merge(X,lz,all.x=TRUE)

# Standardise input
X[,c(-1,-2,-4)]<-data.frame(lapply(X[,c(-1,-2,-4)],stan))

rm(list=setdiff(ls(),"X")) # Housekeeping

# 2) Define models
M1<-function (){
  for(i in 1:N){
    conflict[i]~dbern(yhat[i])
    logit(yhat[i])<-a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]
    fitted[i]<-yhat[i] # Predicted values
    
  }  
  
  # Priors
  a0~dnorm(0,.01)   # Intercept
  b1~dnorm(0,.01)   # Temporal lag
  b2~dnorm(0,.01)   # Spatial lag
  b3~dnorm(0,.01)   # Rainfall
}


M2<-function (){
  for(i in 1:N){
    conflict[i]~dbern(yhat[i])
    logit(yhat[i])<-a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i] +
      b4*pop[i]+b5*lz[i]
    fitted[i]<-yhat[i] # Predicted values
    
  }  
  
  # Priors
  a0~dnorm(0,.01)   # Intercept
  b1~dnorm(0,.01)   # Temporal lag
  b2~dnorm(0,.01)   # Spatial lag
  b3~dnorm(0,.01)   # Rainfall
  b4~dnorm(0,.01)   # Population density
  b5~dnorm(0,.01)   # Livelihood zone
}

M0<-function (){
  for(i in 1:N){
    conflict[i]~dbern(yhat[i])
    logit(yhat[i])<-a0+b1*conflict.l[i]+b2*conflict.w[i]+ b4*pop[i]+b5*lz[i]
    fitted[i]<-yhat[i] # Predicted values
    
  }  
  
  # Priors
  a0~dnorm(0,.01)   # Intercept
  b1~dnorm(0,.01)   # Temporal lag
  b2~dnorm(0,.01)   # Spatial lag
  b4~dnorm(0,.01)   # Population density
  b5~dnorm(0,.01)   # Livelihood zone
}

# 3.1) Estimate models (logit)
X<-X[X$year>=1999,]
N=nrow(X)

# M1
set.seed(42);m1<-jags(data=list(conflict=X$communal,conflict.l=X$communal.l,
                                conflict.w=X$W.communal,rain=X$belg.ma,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","fitted"),
                      model.file=M1,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# M2
set.seed(42);m2<-jags(data=list(conflict=X$communal,conflict.l=X$communal.l,
                                conflict.w=X$W.communal,rain=X$belg.ma,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# 3.2) Analysis logit model
print(m1$BUGSoutput$summary[1:5,c(1,2,3,7)],digits.summary=3) 
print(m2$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) 

beta<-as.vector(m1$BUGSoutput$sims.list$b3)
sum(beta<0)/length(beta)

beta<-as.vector(m2$BUGSoutput$sims.list$b3)
sum(beta<0)/length(beta)

# 4.1) Cross-validation: prepare data
X<-X[order(X$row_name),]

# Indices
S<-as.numeric(factor(X$row_name))
J<-max(S) 
Y<-X$communal

# Vectors
yhat<-yhat2<-beta<-list()
brier<-brier2<-list()

# 4.2) Cross-validation: leave-one-out full model
for(i in 1:J){
  print(i)
  observed=X$communal
  observed[S==i]<-NA # Set observed values to NA for state
  
  # Fit model
  set.seed(42);M<-jags(data=list(conflict=observed,conflict.l=X$communal.l,
                                 conflict.w=X$W.communal,rain=X$belg.ma,
                                 pop=X$p00.km2,lz=X$pastoral,N=N),
                       parameters.to.save=c("b3","fitted"),
                       model.file=M2,n.chains=3,n.iter=2500,
                       n.burnin=1000,n.thin=2)
  
  # Estimated values to matrix/list
  beta[[i]]<-as.vector(M$BUGSoutput$sims.list$b3)
  yhatM<-as.matrix(M$BUGSoutput$sims.list$fitted) 
  yhat[[i]]<-yhatM[,which(S==i)] 
  
}

# 4.3) Cross-validation: leave-one-out, omitting precipitation
for(i in 1:J){
  print(i)
  observed=X$communal
  observed[S==i]<-NA # Set observed values to NA for state
  
  # Fit model
  set.seed(42);M<-jags(data=list(conflict=observed,conflict.l=X$communal.l,
                                 conflict.w=X$W.communal,
                                 pop=X$p00.km2,lz=X$pastoral,N=N),
                       parameters.to.save="fitted",
                       model.file=M0,n.chains=3,n.iter=2500,
                       n.burnin=1000,n.thin=2)
  
  yhatM<-as.matrix(M$BUGSoutput$sims.list$fitted) 
  yhat2[[i]]<-yhatM[,which(S==i)]
  
}

save.image(file='output/csts_regression.RData')


# 5.1) Calculate Brier score
pred<-unlist(lapply(yhat,colMeans))
brier<-(pred-Y)^2

pred2<-unlist(lapply(yhat2,colMeans))
brier2<-(pred2-Y)^2

# 5.2) Plot results
par(mar=c(5,5,3,1),bty='n',las=1,cex.lab=2,cex.axis=2,cex.main=2,pty='s')
plot(jitter(brier2,amount=.02),jitter(brier,amount=.02),
     ylim=c(0,1),xlim=c(0,1),axes=F,col='grey50',
     main='Brier score',xlab='Excluding precipitation',ylab='Including precipitation')
points(brier2[Y==1],brier[Y==1],pch=19)
abline(a=0,b=1,lty=2,lwd=1.5)
axis(1,tick=F,line=-1)
axis(2,tick=F,line=-1)

## FIN