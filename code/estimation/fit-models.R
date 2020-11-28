#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Fit models
# Model estimation as reported in table 1
# Last update 2018 10 23
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source("code/load.R")

# 1) Rainfall anomalies: number of events 
# Baseline model
set.seed(42);m0<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,N=N),
                      parameters.to.save=c("a0","b1","b2","fitted"),
                      model.file=M0,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# General model
set.seed(42);m1<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","fitted"),
                      model.file=M1,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Additional controls
set.seed(42);m2<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# t-test result rather than change 
set.seed(42);m3<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$BEST,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Interaction effects: population density
set.seed(42);m4<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                int=X$p00.km2,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M3,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Interaction effects: pastoral zones
set.seed(42);m5<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                int=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M3,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# 2) Rainfall anomalies: number of fatalities
# General model
set.seed(42);m6<-jags(data=list(conflict=X$nkill.com,conflict.l=X$nkill.com.l,
                                conflict.w=X$W.nkill.com,rain=X$belg.g,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","fitted"),
                      model.file=M1,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Additional controls
set.seed(42);m7<-jags(data=list(conflict=X$nkill.com,conflict.l=X$nkill.com.l,
                                conflict.w=X$W.nkill.com,rain=X$belg.g,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# t-test result rather than change 
set.seed(42);m8<-jags(data=list(conflict=X$nkill.com,conflict.l=X$nkill.com.l,
                                conflict.w=X$W.nkill.com,rain=X$BEST,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Interaction effects: population density
set.seed(42);m9<-jags(data=list(conflict=X$nkill.com,conflict.l=X$nkill.com.l,
                                conflict.w=X$W.nkill.com,rain=X$belg.g,
                                int=X$p00.km2,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M3,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Interaction effects: pastoral zones
set.seed(42);m10<-jags(data=list(conflict=X$nkill.com,
                                 conflict.l=X$nkill.com.l,
                                conflict.w=X$W.nkill.com,rain=X$belg.g,
                                int=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M3,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# 3) Fitted values to list
yhats[[1]]<-data.frame(jagsresults(m1,"fitted"))
yhats[[2]]<-data.frame(jagsresults(m2,"fitted"))
yhats[[3]]<-data.frame(jagsresults(m3,"fitted"))
yhats[[4]]<-data.frame(jagsresults(m4,"fitted"))
yhats[[5]]<-data.frame(jagsresults(m5,"fitted"))
yhats[[6]]<-data.frame(jagsresults(m6,"fitted"))
yhats[[7]]<-data.frame(jagsresults(m7,"fitted"))
yhats[[8]]<-data.frame(jagsresults(m8,"fitted"))
yhats[[9]]<-data.frame(jagsresults(m9,"fitted"))
yhats[[10]]<-data.frame(jagsresults(m10,"fitted"))
yhats[[11]]<-data.frame(jagsresults(m0,"fitted"))

# 4) Some robustness checks
# Drought in neighbouring districts
set.seed(42);r1<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$BEST,
                                W.rain=X$W.BEST,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5","b6",
                                           "fitted"),
                      model.file=M4,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Politically excluded groups
set.seed(42);r2<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                int=X$epr,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M3,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Pastoral livelihood zone more than 50%
set.seed(42);r3<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$BEST,
                                pop=X$p00.km2,lz=X$pastoral50,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Pastoral livelihood zone more than 80%
set.seed(42);r4<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$BEST,
                                pop=X$p00.km2,lz=X$pastoral80,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Pastoral livelihood zone more than 50% (interaction)
set.seed(42);r5<-jags(data=list(conflict=X$nkill.com,conflict.l=X$nkill.com.l,
                                conflict.w=X$W.nkill.com,rain=X$belg.g,
                                int=X$pastoral50,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M3,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Patoral livelihood zone more than 80% (interaction)
set.seed(42);r6<-jags(data=list(conflict=X$nkill.com,conflict.l=X$nkill.com.l,
                                conflict.w=X$W.nkill.com,rain=X$belg.g,
                                int=X$pastoral80,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M3,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Country indicator
set.seed(42);r7<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                kenya=X$kenya,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5","b6",
                                           "fitted"),
                      model.file=M5,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

## Save
save.image(file="output/fitted_models.RData")

## FIN