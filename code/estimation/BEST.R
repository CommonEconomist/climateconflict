#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Bayesian t-test for differences in precipitation means
# Last update 2018 05 18
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
load("data/rainfall.RData")
library(BayesianFirstAid)

# 1) Prepare data
# Focus on two periods: 1981-1998 and 1999-2014. 
# Same period as Pricope et al. (2013)
dat$period<-NA
dat[dat$year>1998,]$period<-1
dat[dat$year>1980 & dat$year<1999,]$period<-0
dat<-na.omit(dat)

N=max(dat$id)
t_spring<-c()

# 2) Run t-tests 
# Can take some time. 
for (i in 1:N){
  print(i)
  
  # Long rains
  x<-dat[dat$id==i & dat$period==0,]$belg.d # 1981-1998
  y<-dat[dat$id==i & dat$period==1,]$belg.d # 1999-2014
  bt<-bayes.t.test(x,y)
  t_spring[i]<-bt$stats[5,7] # Probability that mean is lower for period 1
  
  
}

# 3) Save test results 
ttest_result<-data.frame(row_name=unique(dat$row_name),BEST=t_spring)
save(ttest_result,file="output/ttest.RData")

# 4) Figure: choroplet results
library(RColorBrewer)
library(maptools)

load("raw-data/gaulk_adm2.RData")
districts<-gaul_k[gaul_k$ADM0_NAME=="Ethiopia" | gaul_k$ADM0_NAME=="Kenya",]
districts<-merge(districts,ttest_result)

brks<-c(0,.125,.25,.375,.5,.625,.75,.875,1)
col.regions=brewer.pal(9,"Greys")

## Plot
par(mar=c(0,0,0,0),mgp=c(0,0,0),xpd=NA,mfrow=c(1,1))
plot(districts,
     col=col.regions[findInterval(districts$BEST,brks,all.inside=TRUE)],
     axes=F,main="")
legend(42,5.4, legend=leglabs(brks), fill=col.regions, bty="n",cex=1.3,
       y.intersp=.5)

## FIN