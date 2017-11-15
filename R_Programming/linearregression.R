# Linear regression models
install.packages("ISLR")
?install.packages
library(MASS)
library(ISLR)
?library
?Boston
names(Boston)
summary(Boston)
plot(medv~lstat, Boston)

#simple linear regression model
fit1 = lm(medv~lstat, data = Boston)
?lm
summary(fit1)
abline(fit1, col="red")  #plot linear regression line on the plotted figure
names(fit1)
confint(fit1)   #confidence interval for the coefficients
x = confint(fit1, level = 0.99)
x
?predict
predict(fit1, data.frame(lstat=c(5,10,15)), interval = "confidence", level = 0.99)

fit2 = lm(medv~lstat+age, data =  Boston)
summary(fit2)

fit3 = lm(medv~., data = Boston)
summary(fit3)

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
abline(fit1, col ="red")

#remember: as.factor() change qualitative variables into "factor" (quantitative variables), if the original variables are 
# not "factors", we must do it manually before constructing a model
