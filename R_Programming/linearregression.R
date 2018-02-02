# Linear regression models
install.packages("ISLR") # the package's name "car" has to be quoted
?install.packages
library(MASS)
library(ISLR)
?library
?Boston
names(Boston) # display the names of predictor variables
summary(Boston) # display the min, max, median of each variables
plot(medv~lstat, Boston) # plot medv(y) versus lstat(x) from Boston data

# (1) simple linear regression model
fit1 = lm(medv~lstat, data = Boston) # medv is the response, lstat is the predictor
?lm
summary(fit1) # display residuals, coefficients, RSE, R squared, F-statistic, p-value
names(fit1)

coef(fit1) # get the coefficients of the linear regression
confint(fit1)   #confidence interval for the coefficients
x = confint(fit1, level = 0.99)
x
residuals(fit1) # return residuals
rstudent(fit1) # return studentized residuals
?hatvalues
hatvalues(fit1) # leverage statistics for each predictor variable
which.max(hatvalues(fit1)) # which.max() function identifies the index of the largest element of a vector
summary(fit1)$r.sq # r square to fit1
summary(fit1)$sigma # RSE to fit1

?predict
predict(fit1, data.frame(lstat=c(5,10,15)), interval = "confidence", level = 0.99)
predict(fit1, data.frame(lstat=c(5,10,15)), interval = "prediction", level = 0.99)
predict(fit1)

abline(fit1, lwd=3, col="red")  #plot linear regression line on the plotted figure, line width is 3
?abline
attach(Boston)
plot(lstat, medv, col="red")
plot(lstat, medv, pch="+")
plot(predict(fit1), residuals(fit1)) # shows nn-linearity
plot(predict(fit1), rstudent(fit1))
plot(hatvalues(fit1)) # plot hatvalues(fit1) versus the index of predictors variables

?par
par(mfrow=c(2,2))
plot(fit1) # display 4 plots, residuals vs. fitted value, standardized residuals vs fitted values, residuals vs leverage, S-residuals vs ...

#(2) Multiple linear regression
attach(Boston)
fit2 = lm(medv~lstat+age) # 2 predictors: lstat, age
summary(fit2)

fit3 = lm(medv~., data = Boston) # all predictors
summary(fit3)
r.sq(fit3)
summary(fit3)$r.sq # r square
summary(fit3)$sigma # RSE

??vif
install.packages("car") 
library(car)
vif(fit1)



fit4 = lm(medv~.-age-indus, data = Boston)
summary(fit4)

#linear model with interaction effects
fit5 = lm(medv~lstat*age, data = Boston) #using "lstat*age", three intercepts: lastat, age, lstat:age
summary(fit5)

fit51 = lm(medv~lstat:age, data = Boston)  #using "lstat:age", only one interaction intercept: lstat:age
summary(fit51)

fit6 = lm(medv~lstat+I(lstat^2), data = Boston) #I(x) change the class of object x indicate that it should be treated 'as is'
summary(fit6)
?I

fit7 = lm(medv~poly(lstat,2), data = Boston) # two intercepts: poly(lstat,2)1, poly(lstat,2)2. 
#polynomials of degree 1 to degree over the specified set of points x
summary(fit7)
?poly


#linear model with multiple intercept
?Carseats
names(Carseats)
summary(Carseats)
fit1 = lm(Sales~.+Income:Advertising+Age:Price, data = Carseats)
summary(fit1)

#remember: as.factor() change qualitative variables into "factor" (quantitative variables), if the original variables are 
# not "factors", we must do it manually before constructing a model
