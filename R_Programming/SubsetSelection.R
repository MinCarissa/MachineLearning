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

# Choosing amony models using the validation set approach and cross-validation
