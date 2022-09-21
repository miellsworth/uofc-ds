library(sampling)

data=rbind(matrix(rep("nc",165),165,1,byrow=TRUE),matrix(rep("sc",70),70,1,byrow=TRUE))
data=cbind(data,c(rep(1,100),rep(2,50),rep(3,15),rep(1,30),rep(2,40)),1000*runif(235))
data<-as.data.frame(data)
names(data)=c("state","region","income")

table(data$region,data$state)

s=sampling:::strata(data, stratanames=c("region"), size=c(10,4,6), method="srswor")
mydata=getdata(data,s)
