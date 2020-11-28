#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Table A1: Conditional probabilities
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(DataCombine)

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

# 2.1) Probabilities whole sample (1990-2014)
# Data to vectors
X<-X[X$year>1989,]
violence<-X$communal
violence.l<-X$communal.l
drought<-ifelse(X$belg.d<0,1,0)
drought.l<-ifelse(X$belg.d.l<0,1,0)
large_drought<-ifelse(X$belg.d<=-1,1,0)

# General probabilities
sum(violence)/length(violence) # 0.03
sum(drought)/length(drought)   # 0.59
sum(large_drought)/length(large_drought) # 0.20

# Conditional probabilities
prop.table(table(violence,violence.l),2)    # 0.32
prop.table(table(drought,drought.l),2)      # 0.6
prop.table(table(violence,drought),2)       # 0.04
prop.table(table(violence,drought.l),2)     # 0.03
prop.table(table(violence,large_drought),2) # 0.05

# 2.2) Probabilities pre-period (1990-1998)
# Data to vectors (N=1395)
X2<-X[X$year<1999,]
violence<-X2$communal
violence.l<-X2$communal.l
drought<-ifelse(X2$belg.d<0,1,0)
drought.l<-ifelse(X2$belg.d.l<0,1,0)
large_drought<-ifelse(X2$belg.d<=-1,1,0)

# General probabilities
sum(violence)/length(violence) # 0.02
sum(drought)/length(drought)   # 0.57
sum(large_drought)/length(large_drought) # 0.11

# Conditional probabilities
prop.table(table(violence,violence.l),2)    # 0.30
prop.table(table(drought,drought.l),2)      # 0.6
prop.table(table(violence,drought),2)       # 0.02
prop.table(table(violence,drought.l),2)     # 0.02
prop.table(table(violence,large_drought),2) # 0.01

# 2.3) Probabilities post-period (1999-2014)
# Data to vectors (N=2480)
X2<-X[X$year>1998,]
violence<-X2$communal
violence.l<-X2$communal.l
drought<-ifelse(X2$belg.d<0,1,0)
drought.l<-ifelse(X2$belg.d.l<0,1,0)
large_drought<-ifelse(X2$belg.d<=-1,1,0)

# General probabilities
sum(violence)/length(violence) # 0.04
sum(drought)/length(drought)   # 0.60
sum(large_drought)/length(large_drought) # 0.25

# Conditional probabilities
prop.table(table(violence,violence.l),2)    # 0.32
prop.table(table(drought,drought.l),2)      # 0.60
prop.table(table(violence,drought),2)       # 0.04
prop.table(table(violence,drought.l),2)     # 0.04
prop.table(table(violence,large_drought),2) # 0.06

## FIN