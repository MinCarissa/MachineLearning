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
# boolean vectors can be used to produce a subset of rows or columns of a matrix
Smarket.2005=Smarket[!train,] # a submatrix of the data  in 2005, !train reverse the elements in train
dim(Smarket.2005)
Direction.2005=Direction[!train]
length(Direction.2005)

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


#3. Linear discriminant analysis (LDA)
library(MASS) # lda() function is part of MASS library, we fit a LDA model using the lda() function
train1=Smarket$Year<2005
train1
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=train1) 
lda.fit
#lda() function is similar to lm() and glm() except for the absenct of family option
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
dim(lda.pred$posterior)
lda.class=lda.pred$class
lda.pred$x
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

#Quadratic Discriminant analysis (QDA)
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=train) #qda() similiar to lda()
qda.fit
qda.class=predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

#4. K-Nearest neighbors (KNN)
library(class) #knn() is part of class library

#knn() requires four inputs
train.X=cbind(Lag1,Lag2)[train,] #cbind() column bind into matrix
dim(train.X)
test.X=cbind(Lag1,Lag2)[!train,]
dim(test.X)
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
(83+43)/252

knn.pred=knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

# An application to Caravan insurance data (applying KNN approach)
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822 #Only 6% of people purchased caravan insurance

standardized.X=scale(Caravan[,-86]) #scale() to standardize the data set, such that 
                                    #all variables has a standard deviation of one and a mean of zero
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9) # The fraction of individuals that are correctly predicted to buy insurance is of interest

knn.pred=knn(train.X, test.X, train.Y, k=3)
table(knn.pred,test.Y)
5/26

knn.pred=knn(train.X, test.X, train.Y, k=5)
table(knn.pred,test.Y)
4/15


#Fit a logistic regression model to the data
glm.fit=glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs=predict(glm.fit, Caravan[test,], type="response")
glm.pred=rep("No", 1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

glm.pred=rep("No", 1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)
