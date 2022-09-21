##########################
## MLE: in-class exercises
##########################

# Q1: The following dataset is from a normal distribution, please estimate the mean and sd parameters, 
# (cont.) as well as the variance of estimate
library(ggplot2)

mydata=data.frame(c(7.1507,12.336,-4.0354,8.4487,6.2751,-0.23075,3.2656,6.3705,19.314,16.078,-0.39955,17.14,7.9016,4.7478,7.859,
         4.1801,4.5034,10.959,10.636,10.669,7.686,0.17005,7.869,11.521,6.9556,9.1388,7.9075,3.7862,6.1755,1.8509,8.5536,
         0.41172,0.72452,1.762,-6.7771,10.754,6.3008,1.9803,10.481,-1.8461,4.591,4.0342,6.2768,6.2514,1.5405,4.8798,
         4.3405,7.5108,9.3731,9.4371,1.5454,5.3094,0.14353,0.546,4.9726,11.131,1.9213,6.4855,4.0977,9.4694,0.64374,
         5.1302,7.2101,9.4024,11.177,5.3437,-0.96636,2.0308,0.75367,14.402,2.5376,7.9923,4.2303,8.5544,1.9406,-0.60908,
         -0.6895,6.9528,4.2905,4.2158,10.677,6.1663,5.7912,11.351,1.7821,7.7865,8.3404,4.0251,5.8627,0.33662,0.40819,
         5.4195,7.889,15.342,2.3324,5.7493,4.67,-2.7321,3.2441,-2.1787))

names(mydata) <- "x"

ggplot(mydata, aes(x = x)) + geom_histogram()

LL1<-function(mu,sigma){
  R=dnorm(mydata$x,mu,sigma) # Density of the normal distribution
  return(-sum(log(R))) # Must be negative to find the maximum (mle will find minimum by default)
}

# Use mle function to get the estimate
library(stats4)

# lower = c(mu, sigma), upper = c(mu, sigma)
esti <- mle(LL1, start=list(mu=1,sigma=1), method="L-BFGS-B",lower = c(-Inf, 0), upper = c(Inf, Inf))
esti
summary(esti)
esti_mean <- coef(esti)[1]
esti_sd <- coef(esti)[2]

# Q2: test the nul hypothesis: mean=3 using Wald statistic and likelihood ratio
## Wald statistic
## hint: use "pnorm" function
z <- (esti_mean - 3) / esti_sd
1 - pnorm(z, 0, 1)

## Likelihood ratio
## hint: use "pchisq" function
