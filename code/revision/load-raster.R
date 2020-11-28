#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Preparation for estimation
# Unit-of-analysis: grid-cell
# Compile data, standardise input, load libraries
# Last update 2018 03 22
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Libraries
library(coda)
library(R2jags)
library(rjags)
library(jagstools)

# 1) Load data
load('data/raster_rain.RData')
load('data/raster_population.RData')
load('data/raster_lhz.RData')
load('data/raster_ucdp.RData')

# 2) Merge into data frame
x<-merge(rain,grump,all.x=T) # Precipitation & population
x<-merge(x,lhz,all.x=T)      # Livelihood zones
x<-merge(x,v0[,-2],all.x=T)  # Lagged conflict
colnames(x)[10:11]<-c('events.com.l','nkill.com.l')
x<-merge(x,v[v$period==1,],all.x=T) # Conflict
x$period<-NULL
X<-merge(x,vW,all.x=T)       # Spatial lag conflict

X[is.na(X)]<-0 # Set NAs to 0
X<-X[,c('cell_number','events.com','nkill.com','events.com.l','nkill.com.l',
        'W.events.com','W.nkill.com','belg.g','afuds95ag','pastoral')]

# 3) Standardise input variables
stan<-function(x){(x-mean(x,na.rm=TRUE))/(2*sd(x,na.rm=TRUE))}
X[,c(-1:-3,-10)]<-data.frame(lapply(X[,c(-1:-3,-10)],stan))

rm(list=setdiff(ls(),"X"))

# 4) Settings
options(scipen=4)
set.seed(42); runif(1)
N<-nrow(X)
yhats<-list()

source("code/models.R")

## FIN