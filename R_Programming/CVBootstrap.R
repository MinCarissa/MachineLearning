# The resampling technique

# The validation set approach
library(ISLR)
set.seed(1) # It is a good idea to set a random seed when performing an analysis such as cross-validation that contains an element
            # of randomness, so that the results obtained can be reproduced precisedly at a later time

# linear model
?sample
train=sample(392, 196) # train is a vector of lengt 196, each element is a numeric number smapling from 1:392
length(train)
train
Auto[train,]
Auto[105,]
lm.fit=lm(mpg~horsepower, data=Auto, subset=train) # take the index in train as a training data
lm.fit
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2) # MSE of the observations in the validation set (not in the training set)


#polunomial and cubic regression
#???????????????????????????????????????????????
#lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=train)???poly() can not be used with subset together in the lm()???
#????????????????????????????????????????????????
lm.fit2=lm(mpg~horsepower+I(horsepower^2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3=lm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)  

#chose different training set
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train) 
mean((mpg-predict(lm.fit, Auto))[-train]^2)
?poly

lm.fit2=lm(mpg~horsepower+I(horsepower^2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3=lm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)  

# Leave-one-out cross-validation (LOOCV)
glm.fit=glm(mpg~horsepower, data=Auto) # glm() if without "family=binomial" is the same as lm()
coef(glm.fit)

lm.fit=lm(mpg~horsepower, data=Auto)
coef(lm.fit)

#LOOCV estimate can be computed by glm() and cv.glm()
library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto, glm.fit) #cv.glm() in library boot and can be used together with glm()
cv.err$delta $# The delta vector contain the cross-validaation results

cv.error=rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error # a sharp drop in the estimated test MSE between the linear and quadratic fit, 
         # but then no clear improvement from using higher-order polynomials

# k-fold cross-validation, set K in cv.glm() function
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# The bootstrap, two steps
#Step 1: to create a function that computes the statistic of interest
alpha.fn=function(data, index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))
?sample

#Step 2: boot() function (part of boot libaray), to the perform the bootstrap by repeatedly sampling observation from
# the data set with replacement
boot(Portfolio, alpha.fn, R=1000)
?boot

# Example: using the bootstrap approach to assess the variability of the coefficient estimates
# linear model
boot.fn=function(dataset, index){
  return(coef(lm(mpg~horsepower, data=dataset, subset=index)))
} # Has to include {}, no matter it is a one sentence function
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392,392,replace=T))
boot.fn(Auto, sample(392,392,replace=T))

boot(Auto, boot.fn, 1000) #boot() to compute the standard errors of 1,000 bootstrap estimates for the intercept and slope terms

summary(lm(mpg~horsepower, data=Auto))$coef

#quadratic model
boot.fn=function(dataset, index){
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=dataset, subset=index))
}
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
