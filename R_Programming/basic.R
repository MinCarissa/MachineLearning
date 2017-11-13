getwd()

#1) Vectors
#1.1) define vectors
x = c(2,7,5)
x
?seq
y = seq(10)
y
y = seq(from = 4, to = 10, by = 1)
y
y = seq(from = 4, length =3, by =5)
y
#1.2) operation on vectors
x+y
x/y
x^y
#1.3) Access single or multiple items in vectors (index starting from 1)
x[1]
x[2:3]
#1.4) drop single or multiple items
x[-2]
x[-c(2:3)]

#2)Matrices
#2.1) define matrices
z = matrix(seq(12), 3, 4)
z
#2.2) access elements in matrices
z[1,2]
z[,1]
z[2,]
#2.3)drop elements in matrix
z[-1,]
z[,-c(1:2)]
#2.4)functions
dim(z)
length(z)
ls()
rm(x)
x

#3) generate random numbers
#3.1) number uniform distribution (0 - 1)
x = runif(50)
x
#3.2) generate number normal distribution, mean=?, sd=?
?rnorm
z = rnorm(50)
z
t = rnorm(50, mean = 5, sd = 8)
t

#4)plot figures
#4.1) "plot" function
?plot
plot(x,z)
plot(x,z,xlab = "x label", ylab="y label", main = "title")
#4.2)"hist" function
hist(z)
hist(t)
#4.3) functions
mean(t)
sd(t)
var(t)

#5) reading data
#5.1) load data
AutoData = read.csv("R_Programming/Data/Auto.csv", header = TRUE, na.strings = "?" )
AutoData = na.omit(AutoData)
#functions, plot data
summary(AutoData)
plot(AutoData$mpg, AutoData$weight)
attach(AutoData)
?attach
plot(mpg, weight)
plot(origin, mpg)
# quantity variables become categorical variables
origin = as.factor(origin)
plot(origin, mpg)
pairs(AutoData)
pairs(mpg~cylinders+weight+acceleration)
