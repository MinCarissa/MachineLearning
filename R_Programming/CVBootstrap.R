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


