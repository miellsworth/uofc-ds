# Packages ----
library(dplyr)
library(survey)
library(sampling)
library(ggplot2)

# Construct the same dataset being used in the class
data(swissmunicipalities)
data <- arrange(swissmunicipalities, REG)

for (i in 1:7){
  CC <- cumsum(table(data$REG))
  if (CC[i] > 171 * i){
    data <- data[-seq(171 * i + 1, CC[i]),]
  }
}

# Counts the number of observations in each region, "REG"
table(data$REG)

# Part 1 -----
# Goal: estimate population total
# Q: Can you use "Airind" as auxiliary variable to do ratio estimation ?

# Step 1: check whether the relation is significant
# help("lm")
summary(lm(data$POPTOT ~ data$Airind))
data %>%   filter(POPTOT < 10000) %>%
  ggplot(aes(x = Airind, y = POPTOT)) +
  geom_point() +
  geom_smooth(method = "lm")

# Step 2: check the correlation coefficient (Pearson type)
# help("cor")
cor(data$POPTOT, data$Airind)

# Step 3: Apply ratio estimation 
## tips: take SRS, compute B_hat and then estimate

# Take a sample size of 171 * 3
n = 171 * 3

# Take a subset of the dataframe using simple random sampling
srs <- dplyr::sample_n(data, n, replace = FALSE)

# Calculate the Ratio Estimate
b_hat <- sum(srs$POPTOT) / sum(srs$Airind)
# alternative
# b_hat <- mean(srs$POPTOT) / mean(srs$Airind)

# Calculate the Population Estimate
pop_est <- b_hat * sum(data$Airind)

# Part 2 -----
# Goal: estimate population total
# Q: can you use "Airind" to define strata and then apply stratified sampling to estimate?

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
sum(n)

# do stratified sampling
s = sampling:::strata(newdata2, stratanames=c("stra_name"), size=n, method="srswor")
mydata2=getdata(data,s)


# Part 3 -----
# sampling weight
pw1=l1/n[1]
pw2=l2/n[2]
pw3=l3/n[3]
pw4=l4/n[4]
Total=sum(c(l1,l2,l3,l4))

# Construct a new datafrome
pw<-matrix(c(rep(pw1,n[1]),rep(pw2,n[2]),rep(pw3,n[3]),rep(pw4,n[4])), sum(n),1)
fpc<-matrix(rep(Total,sum(n)),sum(n),1)
mydata2<-cbind(mydata,pw,fpc)  # change mydata to the name of your dataset in part 2

# use svydesign and svyratio to do ratio estimation
# help("svudesign")
# help("svyratio")

