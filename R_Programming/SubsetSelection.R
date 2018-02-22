# Best subset selection

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary)) #is.na() returns a boolean vector, with missing elements "TRUE" values and others "FALSE"
                           # sum() count all of the missing elements
Hitters=na.omit(Hitters) # na.omit() removes all of the rows that have missing values in any variables
dim(Hitters)
sum(is.na(Hitters))

library(leaps) # TO install this package, click "Tools"-> "Install Packages"->"Repository (CRAN)"
regfit.full=regsubsets(Salary~., Hitters) # regsubsets() perform subset selection, where best is quantified using RSS
                                          # syntax of regsubsets() is the same as for lm()
                                          # by default, regsubsets() only reports results up to the best 8-variable model
summary(regfit.full)  # outputs the best subset of variables for each model size
                     # An asterisk indicates that a given variable is included in the corresponding model
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19) # nvmax option returns as many variables as are desired
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary
reg.summary$rsq

par(mfrow=c(2,2))
?plot
plot(reg.summary$rss, xlab="Number of variables", ylab="RSS", type="l") # type="l" connect the plotted points with lines
plot(reg.summary$adjr2, xlab="Number of variables", ylab="Adjusted RSq", type="l")
which.max(reg.summary$adjr2) # identify the location of the maximum point of a vector
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20) # it put points on a plot that has been already created

plot(reg.summary$cp, xlab="Number of variables", ylab="Cp", type="l") # type="l" connect the plotted points with lines
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)

plot(reg.summary$bic, xlab="Number of variables", ylab="BIC", type="l") # type="l" connect the plotted points with lines
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)

par(mfrow=c(1,1))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

coef(regfit.full,6) # optimal model with 6 variables

# Forward and backward stepwise selection
regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# Choosing among models using the validation set approach and cross-validation
set.seed(1)
train=sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
?sample
test=(!train)

#apply to training set
regfit.best=regsubsets(Salary~., data=Hitters[train,], nvmax=19)
test.mat=model.matrix(Salary~., data=Hitters[test,])
# we want to model the observations using the variables to the right of the tilde (~)
# model.matrix will generate a design matrx X ()see: http://genomicsclass.github.io/book/pages/expressing_design_formula.html)
#     1  x_1
#  X= 1  x_2
#     1  x_3

fix(test.mat)
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi    # %*% is for matrix multiplication, only choose the column showing in coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best, 10)

# define our own predict() function, since there is no predict() function for regsubsets.
# check: https://stackoverflow.com/questions/22906723/understand-the-call-function-in-r

predict.regsubsets=function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~., data=Hitters, nvmax=19)
sum=summary(regfit.best)
sum$obj
sum$obj$call[[2]]
coef(regfit.best, 10)

k=10
set.seed(1)
folds=sample(1:k, nrow(Hitters), replace=TRUE)
folds
cv.errors=matrix(NA,k,19, dimnames = list(NULL, paste(1:19)))
?matrix
#===================================================================================
# Hitters dataset has NA value which has to be removed first

best.fit = regsubsets(Salary~., data=Hitters[folds!=1,], nvmax=19)
pred = predict.regsubsets(best.fit, Hitters[folds==1,], id=1)
dim(pred)
pred
length(Hitters$Salary[folds==1])
Hitters[folds==1,]

names(Hitters)

sum(is.na(Hitters$Salary)) #is.na() returns a boolean vector, with missing elements "TRUE" values and others "FALSE"
# sum() count all of the missing elements
Hitters=na.omit(Hitters) # na.omit() removes all of the rows that have missing values in any variables
dim(Hitters)
sum(is.na(Hitters))
fix(Hitters)
#=========================================================================

for(j in 1:k){
  best.fit = regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for(i in 1:19){
    pred = predict.regsubsets(best.fit, Hitters[folds==j,], id=i)
    cv.errors[j,i] = mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors, 2, mean) # apply() average over the column of this matrix
?apply
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')
which.min(mean.cv.errors)

reg.best=regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, 11)

# ridge regression and the lasso (shrinkage methods)

x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary

# Ridge regression
library(glmnet)
?seq
x=seq(10, -2, length=100) # gtom 100 to -2, devided by 100 
x
grid = 10^seq(10, -2, length=100) # vector of length 100
grid
length(grid)
?glmnet
ridge.mod = glmnet(x, y, alpha=0, lambda = grid) # alpha=0 is a ridge regression
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))


ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

predict(ridge.mod, s=50, type="coefficients")[1:20,] # lambda=50


# splitting the samples into a training set and a test set
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
train
test=(-train)
test
y.test = y[test]
y.test

?glmnet
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s=4, newx = x[test,]) # lambda=4
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

ridge.pred = predict(ridge.mod, s=1e10, newx = x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred = predict(ridge.mod, s=0, newx = x[test,], extract = T)
mean((ridge.pred-y.test)^2)

lm(y~x, subset = train)
predict(ridge.mod, s=0, extract = T, type="coefficients")[1:20,]  # equal to linear model (lm())

# choose the tuning parameter lambda
set.seed(1)
# cv.glmnet() perform cross-validation, 10-fold by default
cv.out = cv.glmnet(x[train,], y[train], alpha = 0) # by default, 10 fold cross-validation, 
                                                   # glmnet has its default selected range of lambda
plot(cv.out)
bestlam = cv.out$lambda.min # the value of lambda results in the smallest cross-validation
bestlam

ridge.pred = predict(ridge.mod, s=bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)

out = glmnet(x, y, alpha = 0)
predict(out, type="coefficients", s=bestlam)[1:20.]

# from the ridge regression result, we can see that no coefficients are zero

# The lasso
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

out = glmnet(x, y, alpha=1, lambda = grid)
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:20,]

lasso.coef

# from lasso result, we can see some coefficients estimates are zero

# PCR and PLS regression
# PCR
library(pls)
set.seed(2)
pcr.fit = pcr(Salary~., data = Hitters, scale=TRUE, validation="CV")
# pcr() is similar to lm(), with a few additional options
# scale=TRUE, standardizing each predictor, prior to generating the principal components
# validation="CV" causes pcr() to compute 10-fold cross-validation error 
# for each possible value of M (the number of principle components used)
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP") # plot the cv scorse
                                           # al.type = "MSEP" causes te cv MSE to be plotted

# perform PCR on the training data and evaluate its test set performance
set.seed(1)
pcr.fit = pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred=predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred-y.test)^2)

# apply PCR on the full data set
pcr.fit = pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

# Partial least squares
set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
pls.pred = predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit=plsr(Salary~., data=Hitters, scale=TRUE, ncopm=2)
summary(pls.fit)
