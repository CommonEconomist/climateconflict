#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure 4: Conditional probability
# Using cross-section-time-series data
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Libraries
library(DataCombine)
library(FSA)

# 1) Prepare data
load("data/CenTrend.RData")             # precipitation
dat<-dat[dat$year>=1989,]
dat<-dat[order(dat$row_name,dat$year),]

load("data/communal_violence.RData")    # conflict
X<-merge(dat,communal[,c("row_name","year","communal")],all.x=TRUE)
X$communal[is.na(X$communal)]<-0

# Temporal lag for communal violence 
X<-X[order(X$row_name,X$year),]
X<-slide(X,Var="communal",GroupVar="row_name",NewVar="communal.l",slideBy=-1)

# 2) Pr(violence|drought level)
d<-X[X$year>1989,]
violence<-d$communal
drought.ma<-ifelse(d$belg.ma<0,1,0)

sum(violence)/length(violence) # 0.03
D<-table(violence,drought.ma)
prop.table(D,2) # 0.04

# 2.1) Estimate probabilities
x<-seq(-.6,.6,length=200)
P<-c();lwr<-c();upr<-c()
n<-c();v<-c()

for(i in 1:length(x)){
  # Calculate probability
  MA<-ifelse(d$belg.ma<=x[i],1,0)
  A<-prop.table(table(violence,MA),2)
  P[i]=A[2,2]
  
  # 50% uncertainty interval
  n[i]<-sum(MA)
  v[i]<-sum(violence[MA==1])
  ui<-binCI(v[i],n[i],conf.level=.5,type="exact")
  lwr[i]<-ui[,1];upr[i]<-ui[,2]
}

# 2.2) Plot results
par(bty="n",las=1,cex.lab=2,cex.axis=2,mar=c(5,6,0,0))
plot(x,P,type="n",ylim=c(0,.08),xlim=c(-.6,.6),axes=FALSE,
     ylab="Pr(conflict | precipitation )",
     xlab="Maximum value of moving average")

polygon(c(rev(x),x),c(rev(lwr),upr),
        col="grey80",border=NA) # Uncertainty interval
lines(x,P,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)

# Some statistics
mean(d$belg.ma);sd(d$belg.ma)
max(P)

## FIN