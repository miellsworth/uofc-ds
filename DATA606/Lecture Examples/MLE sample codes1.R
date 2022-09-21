# set seed number
set.seed(1001)

# Maximum likelihood estimation based on "mle" function
# Normal distribution with mean=3 and standard deviation=2
N<-100
x<-rnorm(N,mean=3,sd=2)
print(c(mean(x),sd(x)))

# visualize the data
y=seq(min(x),max(x),length.out=100)
z_point=seq(min(x),max(x),length.out=20)
plot(y,dnorm(y,mean=3,sd=2),ylim=c(0,max(dnorm(y,mean=3,sd=2))),'l',xlab='y',ylab='Probability')
par(new=TRUE)
hist(x,z_point,freq=FALSE,ylim=c(0,max(dnorm(y,mean=3,sd=2))),axes=FALSE,xlab='',ylab='')


# define the (log) likelihood function
LL<-function(mu,sigma){
  R=dnorm(x,mu,sigma) # Density of the normal distribution
  return(-sum(log(R))) # Must be negative to find the maximum (mle will find minimum by default)
}

# Use mle function to get the estimate
library(stats4)

# lower = c(mu, sigma), upper = c(mu, sigma)
esti <- mle(LL, start=list(mu=1,sigma=1), method="L-BFGS-B",lower = c(-Inf, 0), upper = c(Inf, Inf))
esti

# Poisson distribution
M <- 100

# M Random numbers with a poisson distribution
y <- rpois(M, lambda = 5)

LL2<-function(lambda){
  R=dpois(y,lambda)
  return(-sum(log(R)))
}

esti2<-mle(LL2, start = list(lambda=1), method="L-BFGS-B",lower = 0, upper = Inf)
esti2

#####################################
# Maximum likelihood estimation based on "optim" and "hessian" function
library(numDeriv)

# normal distribution
N<-100
x<-rnorm(N,mean=3,sd=2)

LL3<-function(vec){
  R=dnorm(x,vec[1],vec[2])
  return(-log(prod(R)))
}

esti3<-optim(c(1,1),LL3,method="L-BFGS-B",lower = c(-Inf,0), upper = c(Inf, Inf), hessian = TRUE)
hessian(LL3,esti3$par)
1/esti3$heassian[2,2]

# check the variance of estimate using empirical method
T=50
mu_vec=rep(0,T)
sigma_vec=mu_vec

for (i in 1:T){
  N<-100
  x<-rnorm(N,mean=3,sd=2)
  LL3<-function(vec){
    R=dnorm(x,vec[1],vec[2])
    return(-sum(log(R)))
  }
  esti4<-optim(c(1,1),LL3,method="L-BFGS-B",lower = c(-Inf,0), upper = c(Inf, Inf))
  
  mu_vec[i]=esti4$par[1]
  sigma_vec[i]=esti4$par[2]
}

sd(mu_vec)^2
sd(sigma_vec)^2

###################################
###################################
# Poisson 
M<-100
y<-rpois(M,lambda=5)

LL4<-function(lambda){
  R=dpois(y,lambda)
  return(-log(prod(R)))
}

esti5<-optim(1,LL4,method="L-BFGS-B",lower=0, upper=Inf, hessian=TRUE)
hessian(LL4,esti5$par)

# check the variance of estimate using empirical method
T=50
lambda_vec=rep(0,T)

for (i in 1:T){
  M<-100
  y<-rpois(M,lambda=5)
  LL5<-function(lambda){
    R=dpois(y,lambda)
    return(-sum(log(R)))
  }
  esti5<-optim(1,LL5,method="L-BFGS-B",lower=0, upper=Inf)
  
  lambda_vec[i]=esti5$par
}

sd(lambda_vec)^2

######################################
######################################
library(numDeriv)

# Hypothesis test
N<-100
x<-rnorm(N,mean=3,sd=2)

LL3<-function(vec){
  R=dnorm(x,vec[1],vec[2])
  return(-log(prod(R)))
}

# c(1, 1) = starting point, L1 = likelihood function
esti3<-optim(c(1,1),L1,method="L-BFGS-B",lower = c(-Inf,0), upper = c(Inf, Inf), hessian = TRUE)
hessian(LL3, esti3$par)
1/esti3$hessian[2,2]

# In the normal distribution case, test: mean=2
# Wald-type statistic
esti_mean=esti3$par[1]
esti_sd=esti3$par[2]
var_mean=1/esti3$hessian[1,1]
var_sd=1/esti3$hessian[2,2]

z_wald=(esti_mean-2)/sqrt(var_mean)
1-pnorm(z_wald,0,1)


# Likelihood ratio test
L0<-function(sd){
  R=dnorm(x,2,sd)
  return(-sum(log(R)))
}

esti6<-optim(1,L0,method="L-BFGS-B", lower=0, upper=Inf, hessian=TRUE)
esti2_sd=esti6$par
var_sd=1/esti6$hessian

chi_statistic=2*(L0(esti2_sd)-L1(c(esti_mean,esti_sd)))
1-pchisq(chi_statistic,1)