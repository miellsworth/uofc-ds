library(ggplot2)
library(dplyr)
library(readr)

# In class practice problems

# Unit 1 Part 1 ----

# Load data
condominium <- read_csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/condominium.csv")

# Create a multiple linear regression model
price_model <- lm(data = condominium, listprice ~ livingarea + baths + floors + bedrooms)
summary(price_model)

# Solve for the 95% confidence intervals in the coefficients
confint(price_model) # The bedrooms confidence interval covers 0 and should be dropped

# y = b0 + b1 * floors + b2 * livingarea + b3 * baths

# Solve for the 99% confidence intervals in the coefficients
confint(price_model, level = 0.99)  # The floors confidence interval also covers 0 and should be dropped

# y = b0 + b1 * livingarea + b2 * baths

# Partial F-Test to fit the model
# Check floors
reduced_price_model1 <- lm(data = condominium, listprice ~ livingarea + baths + floors)

# Check floors and bedrooms
reduced_price_model2 <- lm(data = condominium, listprice ~ livingarea + baths)

# Test the reduced models to see which 
summary(reduced_price_model1)
summary(reduced_price_model2)
anova(reduced_price_model1, price_model)
anova(reduced_price_model2, price_model)

# Reduced price model 1 will be used

# Predict list price when living area = 1100, floors = 2, bathroom = 1
predict_df <- data.frame(livingarea = 1100/100, floors = 2, baths = 1)
predict(reduced_price_model1, predict_df, interval = "predict")

# Calculate adjusted R squared and RMSE for all possible models
reduced_price_model3 <- lm(data = condominium, listprice ~ baths)
reduced_price_model4 <- lm(data = condominium, listprice ~ baths)


# Unit 1 Part 2 ----

# Create an interaction model with condominium data
price_model_interact <- lm(data = condominium, listprice ~ (livingarea + floors + baths)**2)
summary(price_model_interact)

# The individual P-values are > 0.05 but the R-squared is higher than the reduced model

# We should delete the interactions with the highest P-value first and test how the model responds

# Find the best fitted model for the sales data

# Load data
sales <- read_csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/sales.csv")

# Remove X4 as it is a identifier and should not affect the model results
sales <- sales %>%
  select(-X4)

# Create additive model
sales_model <- lm(data = sales, Y ~ X1 + X2 + X3)
summary(sales_model)

# Remove X1 as P-value > 0.05
sales_model_reduced <- lm(data = sales, Y ~ X2 + X3)
summary(sales_model_reduced)

# Create interaction model
sales_model_interact <- lm(data = sales, Y ~ X2 + X3 + X2 * X3)
summary(sales_model_interact)

# X3 is not significant in this model but since the interaction is, we need to keep the X3 term

# Create the full interaction model
# Create interaction model
sales_model_interact_full <- lm(data = sales, Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
summary(sales_model_interact_full)

# Drop the interaction between X1 and X2 but keep everything else
sales_model_interact_reduced <- lm(data = sales, Y ~ X1 + X2 + X3 + X1 * X3 + X2 * X3)
summary(sales_model_interact_reduced)

# We can see that by reducing the full interaction model, we have a better

# Investigate credit card balance and marital status

# Load data
credit <- read_csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/credit.csv")

# Create a dummy model 
dummymodel <- lm(Balance ~ factor(Married), data = credit)
summary(dummymodel)

# From the summary, Yes = 1, No = 0
# Additionally, since Beta 1 is negative, people who are married have lower balance

# Instead of the rank variable, practice how to interpret the dept variable
salary <- read_csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/salary.csv")
dummymodel <- lm(salary ~ factor(dept),data = salary)
summary(dummymodel)

# From the credit card example, use the lm() function to perform the best fitted model.
# How would you interpret the regression coeffients (if possible)?
# Would you recommend this model for predictive purpose?
summary(lm(data = credit, Balance ~ Income + Limit + Rating + Cards + Age + Education + factor(Gender) +
             factor(Student) + factor(Married) + factor(Ethnicity)))

# Interaction
summary(lm(data = credit, Balance ~ (Income + Limit + Rating + Cards + Age + factor(Student))**2))

# Interaction reduced
# Keep Income*Rating + Income*Age + Income*factor(Student) + Limit* Rating + Limit*factor(Student) +
# Rating*factor(Student) 

summary(lm(data = credit, Balance ~ Income + Limit + Rating + Cards + Age + factor(Student) +
          Income * Rating + Income * Age + Income * factor(Student) + Limit * Rating +
            Limit * factor(Student) + Rating * factor(Student)))

# Interaction FURTHER reduced
# Drop Income*Age, Rating*Student

summary(lm(data = credit, Balance ~ Income + Limit + Rating + Cards + Age + factor(Student) +
             Income * Rating + Income * factor(Student) + Limit * Rating +
             Limit * factor(Student)))

# Physiologist

aerobic <- read_csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/AEROBIC.csv")
simplemodel <- lm(IGG~MAXOXY,data=aerobic)
summary(simplemodel)

quadmodel=lm(IGG~MAXOXY+I(MAXOXY^2),data=aerobic)
summary(quadmodel)

quadmodel2=lm(IGG~MAXOXY+I(MAXOXY^2)+I(MAXOXY^3),data=aerobic)
summary(quadmodel2)

# Model quality as a function of psi

prodqual <- read_csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/PRODQUAL.csv")
prodqual %>%
  ggplot(aes(x = PRESSURE, y = QUALITY)) +
  geom_point() +
  geom_smooth()

simplemodel <- lm(QUALITY ~ PRESSURE, data = prodqual)
summary(simplemodel)

quadmodel <-lm(QUALITY ~ PRESSURE + I(PRESSURE^2), data = prodqual)
summary(quadmodel)

quadmodel2 <-lm(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3), data = prodqual)
summary(quadmodel2)

quadmodel3 <-lm(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3) + I(PRESSURE^4), data = prodqual)
summary(quadmodel3)

quadmodel4 <-lm(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3) + I(PRESSURE^4) + I(PRESSURE^5), data = prodqual)
summary(quadmodel4)

# P-values from the X^5 become insignificant, we should stop at X^4

# From the credit card example, use Stepwise Regression Procedure

library(olsrr)
credit <- read_csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/credit.csv")
credit <- credit[, -1]
full_credit <- lm(data = credit, Balance ~ .)
ols_step_both_p(full_credit, pent = 0.1, prem = 0.3, details = TRUE)

summary(full_credit)

# From the credit card example, use Backward Elimination Procedure

ols_step_backward_p(full_credit, prem = 0.3, details = TRUE)

# From the credit card example, use Forward Selection Procedure

ols_step_forward_p(full_credit, penter = 0.1, details = TRUE)

# All Possible Regressions Selection Procedure

library(leaps)

best.subset<-regsubsets(data = credit, Balance ~., nv=10 )
summary(best.subset)
reg.summary<-summary(best.subset)
rsquare<-c(reg.summary$rsq)
cp<-c(reg.summary$cp)
AdjustedR<-c(reg.summary$adjr2)
RMSE<-c(reg.summary$rss)
cbind(rsquare,cp,RMSE,AdjustedR)
summary(full_credit)

# Lecture example

salary <- read.csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/EXECSAL2.csv", header = TRUE )
firstordermodel<-lm(Y~X1+X2+factor(X3)+X4+X5,data=salary)
summary(firstordermodel)
interacmodel<-lm(Y~(X1+X2+factor(X3)+X4+X5)^2,data = salary)
summary(interacmodel)
bestinteracmodel<-lm(Y~X1+X2+factor(X3)+X4+X5+factor(X3)*X4,data=salary)
summary(bestinteracmodel)
library(GGally)
salarydata <- data.frame(salary$Y,salary$X1,salary$X2,salary$X3,salary$X4,salary$X5)
ggpairs(salarydata, lower = list(continuous = "smooth_loess",
                                 combo = "facethist",
                                 discrete = "facetbar",
                                 na = "na"))
bestmodel<-lm(Y~X1+I(X1^2)+X2+factor(X3)+X4+X5+factor(X3)*X4,data=salary)
summary(bestmodel)
bestmodel2 <-lm(Y~X1+I(X1^2)+I(X1^3)+X2+factor(X3)+X4+X5+factor(X3)*X4,data=salary)
summary(bestmodel2)
#P-value for X^3 term is not significant, we shouldn't use this model

# Clerical in class problem
clerical <- read_csv("/Users/Ellsworth/Documents/School/DATA603/Lectures/CLERICAL.csv")
clerical <- clerical[, -c(1, 2)]
clerical_full <- lm(data = clerical, Y ~.)
summary(clerical_full)

clerical_reduced <- lm(data = clerical, Y ~ X2 + X4 + X5)
summary(clerical_reduced)

# Stepwise
ols_step_both_p(clerical_full, pent = 0.1, prem = 0.3, details = TRUE)

# All-Possible-Regressions
best.subset<-regsubsets(data = clerical, clerical_full, nv=10 )
summary(best.subset)
reg.summary<-summary(best.subset)
rsquare<-c(reg.summary$rsq)
cp<-c(reg.summary$cp)
AdjustedR<-c(reg.summary$adjr2)
RMSE<-c(reg.summary$rss)
cbind(rsquare,cp,RMSE,AdjustedR)
summary(full_clerical)
