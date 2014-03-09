library(MASS)
library(ISLR)
##### Simple linear regression
names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")#draw any line
names(fit1)
#confidence interval for the coefficient estimates
confint(fit1)
#produce confidence intervals and prediction intervals
#for the prediction of medv for a given value of lstat
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")
##### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

plot(predict(fit3), residuals(fit3))
#rstudent studentized residuals
plot(predict(fit3), rstudent(fit3))

#On the basis of the residual plots, there is some evidence of non-linearity
#Leverage statistics can be computed for any number of predictors
plot(hatvalues(fit3))
which.max(hatvalues(fit3))#index of the largest element of the vector

#gives us the R2
summary(fit3)$r.sq
#gives us the RSE
summary(fit3)$sigma

#exclude variable -age-indus from calc
fit4=update(fit3,~.-age-indus)
summary(fit4)

### Nonlinear terms and Interactions
#lstat*age simultaneously includes lstat, age, and the 
#interaction term lstat×age as predictors - lstat+age+lstat:age
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)
fit61=lm(medv~lstat ,Boston); summary(fit61)

#anova() function performs a hypothesis
# test comparing the two models. The null hypothesis is that the two models
# fit the data equally well, and the alternative hypothesis is that the full
# model is superior. Here the F-statistic is 135 and the associated p-value is
# virtually zero. This provides very clear evidence that the model containing
# the predictors lstat and lstat2 is far superior to the model that only
# contains the predictor lstat. This
anova(fit61 ,fit6)

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
fit8=lm(medv~log(lstat))
points(lstat,fitted(fit8),col="green",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)

###Qualitative predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
#returns the coding that R uses for the dummy variables.
contrasts(Carseats$ShelveLoc)
###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)




