library(survey)
library(sampling)
data(api)

tail(apipop)
temp=apipop[,36]
ind=which(temp %in% NA)
mydata<-apipop[-ind,]

E_total<-sum(mydata$stype=="E")
M_total<-sum(mydata$stype=="M")
H_total<-sum(mydata$stype=="H")

# take 100 E, 50 M and 50 H from mydata
pw1<-E_total/100
pw2<-M_total/50
pw3<-H_total/50

mean_sd<-function(T){
  temp2=rep(0,T)
  for(i in 1:T){
    s<-sampling:::strata(mydata, stratanames=c("stype"), size=c(50,50,100), method="srswor")
    newdata<-getdata(mydata,s)
    temp2[i]=sum(newdata$api.stu)/sum(newdata$enroll)
  }
  return(temp2)
}
result=mean_sd(10)
mean(result)
sd(result)


# In apistrat, the strata is school type. 100 E, 50 M and 50 H are selected.
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

## domain means are ratio estimates, but available directly
svyratio(~I(api.stu*(comp.imp=="Yes")), ~as.numeric(comp.imp=="Yes"), dstrat)
svymean(~api.stu, subset(dstrat, comp.imp=="Yes"))

## separate and combined ratio estimates of total
(com<-svyratio(~api.stu, ~enroll, dstrat))
(sep<-svyratio(~api.stu, ~enroll, dstrat, separate=TRUE))

stratum.totals<-list(E=1877350, H=1013824, M=920298)
predict(sep, total=stratum.totals)
predict(com, total=sum(unlist(stratum.totals)))



