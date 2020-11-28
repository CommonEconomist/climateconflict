#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Estimation for different conflict types
# Last update 2018 05 11
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source("code/load.R")

# 1) Estimate models
# All conflict
set.seed(42);c0<-jags(data=list(conflict=X$events,conflict.l=X$events.l,
                                conflict.w=X$W.events,rain=X$belg.g,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)
print(c0)

# State-based conflict
set.seed(42);c1<-jags(data=list(conflict=X$events.gov,
                                conflict.l=X$events.gov.l,
                               conflict.w=X$W.events.gov,rain=X$belg.g,
                               pop=X$p00.km2,lz=X$pastoral,N=N),
                     parameters.to.save=c("a0","b1","b2","b3","b4","b5"),
                     model.file=M2,n.chains=3,n.iter=2500,
                     n.burnin=1000,n.thin=2)
print(c1)

# One-sided violence
set.seed(42);c2<-jags(data=list(conflict=X$events.one,
                                conflict.l=X$events.one.l,
                               conflict.w=X$W.events.one,rain=X$belg.g,
                               pop=X$p00.km2,lz=X$pastoral,N=N),
                     parameters.to.save=c("a0","b1","b2","b3","b4","b5"),
                     model.file=M2,n.chains=3,n.iter=2500,
                     n.burnin=1000,n.thin=2)
print(c2)


save.image(file='output/conflict_types.RData')

## FIN