#Classification: Logistic regression, linear discriminant analysis, KNN

#1. The stock market data
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9]) # produce a matrix that contains pairwise correlations among the predictors, cannot contain qualitative predictors
attach(Smarket)
plot(Volume)
?plot

#2. Logistic regression
fit1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial) #"family= binomial" to show logistic regression
summary(fit1)
coef(fit1) # to access the coefficients for this fitted model
summary(fit1)$coef # smmary function plus $ to access particular aspects of the model
summary(fit1)$coef[,-4]

glm.probs=predict(fit1, type="response") # type="response" means, the probability of the form P(Y=1|X)
glm.probs[1:10] # By default, the probabilities are computed for the training data, if not other data set is supplied
contrasts(Direction) # a dummy variable with a 1 for Up

?rep
glm.pred=rep("Down",1250) # It creates a vector of 1,250 Down elements
glm.pred[glm.probs>.5]="Up" # it transforms to "Up" all of the elements for which the predicted probability increase exceeds 0.5
table(glm.pred, Direction) # it creates a confusion matrix 
mean(glm.pred == Direction) # it compute the fraction of days for which the prediction was correct

train=(Year<2005) # a boolean vector of 1,250 elements, <2005 is true, otherwise falise
Smarket.2005=Smarket[!train,] # a submatrix of the data  in 2005, !train reverse the elements in train
dim(Smarket.2005)
Direction.2005=Direction[!train]

#Using "subset" argument to build a model on date before 2005, and test model on the days in 2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005, type="response")

# compare the predicted value to true value of days in 2005
glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction.2005) # compare the predicted value to true value of days in 2005
mean(glm.pred==Direction.2005)

#refit the logistic regression using La1 and Lag2
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005, type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction.2005) 
mean(glm.pred==Direction.2005)

#predict particular values of Lag1 and Lag2
predict(glm.fit, newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type="response")


#3. Linear discriminant analysis

