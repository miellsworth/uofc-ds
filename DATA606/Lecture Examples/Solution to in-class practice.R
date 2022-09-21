library(survey)
library(sampling)

data(swissmunicipalities)
data<-swissmunicipalities[order(swissmunicipalities$REG),]
for (i in 1:7){
  CC<-cumsum(table(data$REG))
  if (CC[i]>171*i){
    data<-data[-seq(171*i+1,CC[i]),]
  }
}
table(data$REG)

####################### Practice 1 ##########################
# Use ratio estimation
# check the relation between POPTOT (municipality population) and Airind (industrial area)
fit1<-lm(POPTOT~Airind, data=data)
summary(fit1)

# Compute the correlation coefficient
cor(data$POPTOT, data$Airind)

# Use ratio estimation
# option 1
# Take an SRS and estimate
n=171*3
ind<-sample(seq(1:dim(data)[1]),n)
newdata<-data[ind,]
B_hat=sum(newdata$POPTOT)/sum(newdata$Airind)
esti_POPTOT=B_hat*sum(data$Airind)


###################### Practice 2 ############################
# Use stratified sampling
scatter.smooth(x=data$Airind, y=data$POPTOT, main="Population - industrial area")
data<-data[order(data$Airind),]
summary(data$Airind)

l1=sum(data$Airind<=2)
l2=sum(data$Airind<=8)-l1
l3=sum(data$Airind<=18)-l1-l2
l4=length(data$Airind)-l1-l2-l3
print(c(l1,l2,l3,l4))

stra_name<-matrix(c(rep("L1",l1),rep("L2",l2),rep("L3",l3),rep("L4",l4)), length(data$Airind),1)
newdata2<-cbind(stra_name,data)

# sample size in each strata
n=round(c(l1,l2,l3,l4)*171*3/length(data$Airind))
N=cumsum(n)
sum(n)

# take stratified sampling
s=sampling:::strata(newdata2, stratanames=c("stra_name"), size=n, method="srswor")
mydata=getdata(data,s)
esti_poptot_mean=mean(mydata$POPTOT[1:N[1]])*n[1]/sum(n)+mean(mydata$POPTOT[(N[1]+1):N[2]])*n[2]/sum(n)+mean(mydata$POPTOT[(N[2]+1):N[3]])*n[3]/sum(n)+mean(mydata$POPTOT[(N[3]+1):N[4]])*n[4]/sum(n)
esti_poptot=esti_poptot_mean*length(data$POPTOT)

# sampling weight
pw1=l1/n[1]
pw2=l2/n[2]
pw3=l3/n[3]
pw4=l4/n[4]
Total=sum(c(l1,l2,l3,l4))

# Construct a new datafrome
pw<-matrix(c(rep(pw1,n[1]),rep(pw2,n[2]),rep(pw3,n[3]),rep(pw4,n[4])), sum(n),1)
fpc<-matrix(rep(Total,sum(n)),sum(n),1)
mydata2<-cbind(mydata,pw,fpc)

# use svydesign and svyratio to do ratio estimation
dstrat<-svydesign(id=~1,strata=~stra_name, weights=~pw, data=mydata2, fpc=~fpc)
com<-svyratio(~POPTOT, ~Airind, dstrat)
predict(com, total=sum(data$Airind))

