a=read.csv(file.choose(), header=TRUE)

shapiro.test(a$commits)
#change the owner type column, ORG->1, USR->0
dummyOwnerType<-NULL
dummyOwnerType<-(a$OwnerType=="ORG")*1
a1<-cbind(dummyOwnerType, a)

# Random select 10 sample from the new dataset
a_random=a1[a1$prjId %in% sample(unique(a1$prjId),10),]
a12<-a[a$prjId %in% c(2647, 3671, 3721, 5624, 8462, 10797, 12558,
                      36946, 57651, 1417262, 10163575, 11208841), ]

library(lattice)
xyplot(commits~Time | prjId, data=a12, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, ylim=c(-100, 1000), as.table=T)

# Check the missing value
sum(is.na(a1))

# Outliers - use owner type and health to select the dataset
# could also use dummyOwnerType column-> ORG=1, USR=0
outlier1=boxplot(Health~dummyOwnerType,data = a,
                 xlab="Owener Type",ylab="Health")$out
out1<-a1[a1$OwnerType=='ORG' & (a1$Health==83 | a1$Health==0 | a1$Health==100),]
print(out1)
out2<-a1[a1$OwnerType=='USR' & (a1$Health==83 | a1$Health==66),]
print(out2)
# Combine the outliers
outAll<- rbind(out1,out2)
print(outAll)
# Remove the outliers
a2<- a1[-which(a$prjId %in% outAll$prjId),]

#Assumption checks - visualization (normality)
sum(is.na(a2))
hist(log(a2$commits+1))
hist(a2$commits)

#Model A - Unconditional means model
library(nlme)
model.a <- lme(log(commits+1) ~ 1, a2, random= ~1 |prjId)
summary(model.a)
VarCorr(model.a)
m<- VarCorr(model.a)
icc.a<-as.numeric(m[1,1]) / (as.numeric(m[1,1]) + as.numeric(m[2,1]))
print(icc.a)
#Model B - unconditional growth model
model.b <- lme(log(commits+1) ~ Time , data=a2, random= ~ Time | prjId, method="ML")
summary(model.b)
VarCorr(model.b)

# show the intercept[1] and slope[2] of model B
fixef.b <- fixef(model.b) 
print(fixef.b)
# Time: 1-8 (total 8)
fit.b <- fixef.b[[1]] + a2$Time[0:8]*fixef.b[[2]]
print(fit.b)
plot(a2$Time[0:8], fit.b, ylim=c(0, 5), type="b", 
     ylab="predicted commit", xlab="Time")   
title("Model B \n Unconditional growth model")
# get variance 
VarCorr(model.b)

#Model C
model.c <- lme(log(commits+1) ~ dummyOwnerType*Time , data=a2, random= ~ Time | prjId, method="ML")
summary(model.c)
#dummyOwnerType
# Owner type : Time - effect of owner type over period of time
fixef.c <- fixef(model.c)
fit.c0 <- fixef.c[[1]] + a2$Time[0:8]*fixef.c[[3]]
fit.c1 <- fixef.c[[1]] + fixef.c[[2]] + a2$Time[0:8]*fixef.c[[3]] + a2$Time[0:8]*fixef.c[[4]]

plot(a2$Time[0:8], fit.c0, ylim=c(0, 6), type="b",col='red', 
     ylab="predicted commit", xlab="Time")
lines(a2$Time[0:8], fit.c1, type="b", pch=17)   
title("Model C \n Uncontrolled effects of commit") 
legend("topleft", inset=.05,
       c("ORG=black", "USR=red"))

#Model D
model.d <- lme(log(commits+1) ~ dummyOwnerType*Time + log(issueCmnt+1)*Time , data=a2, random= ~ Time | prjId, method="ML")
summary(model.d)

#Model E
model.e <- lme(log(commits+1) ~ dummyOwnerType + log(issueCmnt+1)*Time , data=a2, random= ~ Time | prjId, method="ML")
summary(model.e)
