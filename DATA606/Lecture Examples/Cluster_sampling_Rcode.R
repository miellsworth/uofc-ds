library(sampling)
library(dplyr)

# Attach data, check the class of data and the first 6 rows of data
data(swissmunicipalities)
class(swissmunicipalities)
head(swissmunicipalities)

# How many units are in each region (clusters are defined as per region)
table(swissmunicipalities$REG)
data <- swissmunicipalities %>%
  arrange(REG)
head(data)

# Construct equal-size cluster
data <- data[-seq(172,589),]

table(data$REG)
for (i in 2:7){
  CC<-cumsum(table(data$REG))
  if (CC[i]>171*i){
    data<-data[-seq(171*i+1,CC[i]),]
  }
}
table(data$REG)

# Now the data is artificial
# A histogram of total population of each unit
hist(data$POPTOT)

ind=as.numeric(data$POPTOT<=100000)
newdata<-swissmunicipalities[which(ind %in% 1),]
hist(newdata$POPTOT)

ind2=as.numeric(newdata$POPTOT<=20000)
newdata2<-newdata[which(ind2 %in% 1),]
hist(newdata2$POPTOT)

# Population mean and sd
mean(data$POPTOT)
sd(data$POPTOT)
sum(data$POPTOT)

# One-stage cluster sampling
cl=sampling:::cluster(data,clustername=c("REG"),size=3,method="srswor")
mydata=getdata(data, cl)
head(mydata)
mean(mydata$POPTOT)
sd(mydata$POPTOT)
mean(mydata$POPTOT)*length(data[,1])

# population mean variance estimate
ti_var<-sd(c(sum(mydata[1:171,21]),sum(mydata[172:342,21]),sum(mydata[343:513,21])))^2
(1-3/7)*ti_var/3/171^2  

mean_vec<-function(T){
  esti_mean<-rep(0,T)
  for (i in 1:T){
    cl=sampling:::cluster(data,clustername=c("REG"),size=3,method="srswor")
    mydata=getdata(data, cl)
    esti_mean[i]=mean(mydata$POPTOT)
  }
  return(esti_mean)
}
sd(mean_vec(20))^2

# SRS
COM_sample=sample(data$COM, 171*3, replace=FALSE)
ind3=which(data$COM %in% COM_sample)
POPTOT_sample=data[ind3,22]
mean(POPTOT_sample)
sd(POPTOT_sample)
mean(POPTOT_sample)*length(data[,1])

mean_vec2<-function(T){
  esti_mean<-rep(0,T)
  for (i in 1:T){
    COM_sample=sample(data$COM, 171*3, replace=FALSE)
    ind3=which(data$COM %in% COM_sample)
    POPTOT_sample=data[ind3,22]
    esti_mean[i]=mean(POPTOT_sample)
  }
  return(esti_mean)
}
sd(mean_vec2(20))^2

# The SRS seems to be more stable, why?
within_var=rep(0,7)
within_mean=rep(0,7)
for (i in 1:7){
  within_var[i]=sd(data[(171*(i-1)+1):(171*i),22])^2
  within_mean[i]=mean(data[(171*(i-1)+1):(171*i),22])
}
MSW=mean(within_var)
MSB=sd(within_mean)^2
print(c(MSW, MSB))

# Two-stage cluster sampling
TS_sample<-mstage(data,stage=list("cluster",""), varnames=list("REG",""), size=list(3,c(10,10,10)), method=list("srswor","srswor"))
mydata2<-getdata(data,TS_sample)[[2]]
head(mydata2)
