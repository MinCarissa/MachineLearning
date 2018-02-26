# Non-linear modeling

library(ISLR)
attach(Wage)

# Polynomial regression and step functions
# build linear model
fit = lm(wage~poly(age, 4), data=Wage) # it returns matrix, each column are a basis of orthogonal polynomials (age, age^2, age^3, age^4)
coef(summary(fit))

# to obtain age, age^2, age^3, age^4 directly, use option "raw=TRUE"
fit2 = lm(wage~poly(age, 4, raw=T), data=Wage)
coef(summary(fit2))

fit2a = lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)  # get the same formula as fit2, I() is the wrapper function
coef(fit2a)

fit2b = lm(wage~cbind(age, age^2, age^3, age^4), data=Wage) # get the same formula as fit2a, cbind() is the wrapper function
                                                            # cbind() building a matrix from a collection of vectors
coef(fit2b)

#predict according to the linear model
agelime = range(age)
agelime
age.grid = seq(from = agelime[1], to = agelime[2])
preds = predict(fit, newdata =list(age = age.grid), se = TRUE)  #wants standard error
se.bands = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands

# plot the predicts
par(mfrow=c(1, 2), mar=c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
?par
?mfrow
# par can be used to set or query graphical parameters
# mar: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be 
#      specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.
# oma: A vector of the form c(bottom, left, top, right) giving the size of the outer margins in lines of text.

plot(age, wage, xlim = agelime, cex=.5, col="darkgrey")
# xlim: the x limits (x1, x2) of the plot. 
# cex: a numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default. 
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3) # lty is line types
                                                       # plot the columns of one matrix against the columns of another
?matlines

preds2 = predict(fit2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit-preds2$fit))

 # deciding on the degree of the polynomial to use
fit.1=lm(wage~age, data=Wage)
fit.2=lm(wage~poly(age, 2), data=Wage)
fit.3=lm(wage~poly(age, 3), data=Wage)
fit.4=lm(wage~poly(age, 4), data=Wage)
fit.5=lm(wage~poly(age, 5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5) # anova() works whether or not we used orthogonal polynomials
coef(summary(fit.5))

# anova() also works when we have other terms in the model as well
fit.1 = lm(wage~education+age, data=Wage)
fit.2 = lm(wage~education+poly(age, 2), data=Wage)
fit.3 = lm(wage~education+poly(age, 3), data=Wage)
anova(fit.1, fit.2, fit.3)


# predicting an individual earns more than $250,00 per year
fit = glm(I(wage>250)~poly(age, 4), data=Wage, family = binomial)
# I() in glm() function create the binary response variable on the fly.
# wage>250 create logical variables, TRUEs to 1, and FALSEs to 0.
preds = predict(fit, newdata=list(age=age.grid), se=T)
# The default glm() model is  type="link", we get predictions for the logit, log(Pr(Y=1|X)/(1-Pr(Y=1|X))) = XB
# Pr(Y=1|X) = exp(XB)/(1+exp(XB))
pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

# We could have directly computed the probabilities by selecting the type="response" in the predict() function
preds = predict(fit, newdata = list(age=age.grid), type="response", se=T)

plot(age, I(wage>250), xlim=agelime, type="n", ylim = c(0, 2))
points(jitter(age), I((wage>250)/5), cex=.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

# fit a step function
table(cut(age,4)) # cut age into 4 ranges, cut() returns an ordered categorical variable
fit = lm(wage~cut(age, 4), data=Wage)
coef(summary(fit))

#splines
