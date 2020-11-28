#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Table 1: regression results
# Main results based on fit-models.R
# Last update: 2018 05 16
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
options(scipen=1)

# Libraries
library(R2jags)
library(rjags)
library(jagstools)

# 1.1) Table 1: Regression results
load("output/fitted_models.RData")

# Number of events
print(m0$BUGSoutput$summary[1:4,c(1,2,3,7)],digits.summary=3) # Col. 1
print(m1$BUGSoutput$summary[1:5,c(1,2,3,7)],digits.summary=3) # Col. 2
print(m2$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 3
print(m3$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 4
print(m4$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 5
print(m5$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 6

# Number of fatalities
print(m7$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 7
print(m8$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 8

# 1.2) Estimated effects
# Estimated effect conflict dynamics
exp(mean(m0$BUGSoutput$sims.list$b1))
exp(mean(m0$BUGSoutput$sims.list$b2))

# P(negative effect rainfall)
beta<-as.vector(m1$BUGSoutput$sims.list$b3)
sum(beta<0)/length(beta)
print(m1$BUGSoutput$summary[4,c(1,2,4,6)],digits.summary=3)

# Population density and interaction
b5<-as.vector(m4$BUGSoutput$sims.list$b5) # interaction
sum(b5<0)/length(b5)
b4<-as.vector(m4$BUGSoutput$sims.list$b4) # direct
sum(b4<0)/length(b4)

# Livelihood zones
exp(mean(m2$BUGSoutput$sims.list$b5))
exp(m2$BUGSoutput$summary[6,c(3,7)]) 
exp(m2$BUGSoutput$summary[6,c(4,6)]) 

b5<-as.vector(m5$BUGSoutput$sims.list$b5) # interaction
sum(b5<0)/length(b5)

# BEST
b3<-as.vector(m3$BUGSoutput$sims.list$b3)
sum(b3>0)/length(b3)

# Robustness check: Kenya
print(r7$BUGSoutput$summary[1:8,c(1,2,3,7)],digits.summary=3) 
beta<-as.vector(r7$BUGSoutput$sims.list$b6)
sum(beta>0)/length(beta)

beta<-as.vector(r7$BUGSoutput$sims.list$b3)
sum(beta<0)/length(beta)

# Robustness check: ethnic groups
print(r2$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) 

b4<-as.vector(r2$BUGSoutput$sims.list$b4) # main
sum(b4>0)/length(b4)

b5<-as.vector(r2$BUGSoutput$sims.list$b5) # interaction
sum(b5<0)/length(b5)

# 1.3) Robustness checks
# Drought neighbouring districts
print(r1$BUGSoutput$summary[1:8,c(1,2,3,7)],digits.summary=3) 
print(r1$BUGSoutput$summary[7,c(1,2,4,6)],digits.summary=3)
b6<-as.vector(r1$BUGSoutput$sims.list$b6) 
sum(b6<0)/length(b6)

# Excluded ethnic groups
print(r2$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) 

# Pastoral livelihoods
print(r3$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) 
print(r4$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) 

print(r5$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3)
print(r6$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3)

## FIN