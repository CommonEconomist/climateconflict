#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Preparation for estimation
# Compile data, standardise input, load libraries
# Last update 2018 03 30
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# 1) Merge data
load("data/lzones.RData")
load("data/ged.RData")
load("data/rainfall.RData")
load("data/pop_dens.RData")
load("output/ttest.RData")
load("data/epr_d.RData")

# Merge into dataframe
X<-merge(ged,rain)
X<-merge(X,pop.dens)
X<-merge(X,ttest_result)
lz<-data.frame(row_name=adm2$row_name,pastoral=adm2$pastoral,
               pastoral50=adm2$pastoral50,pastoral80=adm2$pastoral80)
X<-merge(X,lz)
X<-merge(X,epr)

# Dummy variables for BEST
X$best_50<-ifelse(X$BEST>.66,1,0)
X$best_80<-ifelse(X$BEST>.9,1,0)

# Per 1000 conflict measure
X$com.pc<-X$nkill.com/X$p00*10000

#### Check ####
mean(X$belg.g[X$communal==1])
mean(X$belg.g[X$communal==0])

# 2) Calculate additional spatial lags
# Adjacency matrix
library(spdep)
k<-poly2nb(adm2,queen=TRUE) 
W<-nb2listw(k,style="W",zero.policy=TRUE) # Average
Wb<-nb2listw(k,style="B",zero.policy=TRUE) # Sum
adj<-as.matrix(listw2mat(W))
adjb<-as.matrix(listw2mat(Wb))
ident<-diag(1,nrow=1)

# Calculate lags
X$W.BEST<-as.numeric((ident%x%adj)%*%X$BEST)
X$W.pastoral<-as.numeric((ident%x%adjb)%*%X$pastoral) 

# Save data
save("X",file="output/markov.RData")

# 3) Standardise input variables 
belg.g<-X$belg.g

# Standardise
stan<-function(x){(x-mean(x,na.rm=TRUE))/(2*sd(x,na.rm=TRUE))}
X[,c(-1:-15,-62:-68)]<-data.frame(lapply(X[,c(-1:-15,-62:-68)],stan))
X$mamj<-belg.g

# Dummy for Kenya
X$kenya<-ifelse(X$row_name %in% adm2$row_name[adm2$ADM0_NAME=="Kenya"],1,0)
table(X$kenya)

rm(list=setdiff(ls(),"X"))

# 4) Settings 
options(scipen=4)
set.seed(42); runif(1)
N<-nrow(X)
yhats<-list()

# 5) Load libraries and models 
library(coda)
library(R2jags)
library(rjags)
library(jagstools)

source("code/models.R")

## FIN