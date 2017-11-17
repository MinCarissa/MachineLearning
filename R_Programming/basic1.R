# vector
y <- c(1,3,2,5) # function c() (for concatenate)
y
x = c(1,6,2)
x
length(x)

# a sequence of numbers
x=seq(1, 10) # a start from 1, end with 10, increase by 1 each time
x=1:10  # =seq(1, 10)
x = seq(-pi, pi, length=5)
x = seq(1, 6, by=2) # start from 1, increate 2 each time, last element <=6

x# Hine: hitting up or down arrow in Console, will show previous or next command

ls() # show a list of all the objects up to now
rm(x)
ls()
rm(list = ls()) # remove all objects, notice parameter "list"
ls()


#matrix
z = matrix(data=c(1,2,3,4), nrow=2, ncol=2) # by default, R creates matrices by successively filling in columns
z
z = matrix(data=c(1,2,3,4), 2, 2) # the 1st, 2nd and 3rd parameters all can be ommitted
z
z = matrix(data=c(1,2,3,4), 2, 2, byrow = TRUE) # parameter "byrow", filling element in rows
z
z = matrix(data=seq(from=2, to=10, length=4), 2, 2)
z
length(z) # return the number of entries in a matrix
sqrt(z) # return the square root of each element
z^2 # return the square of each element

#indexing data
A = matrix(1:16, 4, 4)
A
A[2,3]
A[c(1,3), c(2,4)] # select the index row and columns
A[1:3, 2:4] # select row and columns from to
A[1,] #  R treats a single row or column of a matrix as a vector
A[-c(1),] # minue can only work with vectors
dim(A)
length(A)

#random numbers
x = rnorm(50) #by default, rnorm() generates a vector of random normal variables, with a mean of 0 and a standard deviation of 1.
y= x + rnorm(50, mean=50, sd=.1) # rnorm() has more parameters
cor(x, y) # calculate correlation between x and y

rnorm(3)
rnorm(3) # by default, rnorm() generate a difference vector each time
set.seed(1000) # in order to reproduce the same vector, we use set.seed() function
rnorm(3)
set.seed(1000)
x=rnorm(3)
mean(x) # mean of vector x
sd(x) # standard deviation of vector x
var(x) # variance of vector x
sqrt(var(x)) # = sd(x)


# Graphics
# plot() function
x = rnorm(100)
y = rnorm(100)
plot(x, y, xlab="x-axis", ylab="y-axis", main="plot x-y") # parameters: x, y, xlab="", ylab="", main="", col=""

#save output of plot
pdf("figure.pdf") # create a pdf file
plot(x, y, col="green")
dev.off() # indicates to R that we are done creating the plot

jpeg("figure.jpeg") # create a jpeg file
plot(x, y, col="red")
dev.off() 

#contour() function to display 3-dimensional data, paramters: x, y, z (value for each pair of (x,y))
x = seq(1, 10)
y = x
f = outer(x, y, function(x,y)cos(y)/(1+x^2))
#function(): define new function in R
contour(x, y, f)
contour(x, y, f, nlevels=45, add=T) # nlevels: number of contour levels; add = T (TRUE, add to the current plot)
fa = (f-t(f))/2
contour(x, y, fa, nlevels=15)

#image() works the same way as contour(). except that it produces a color-coded plot whose colors depedend on the z value
image(x, y, fa)

#persp() function produce a 3-dimensional plot
persp(x, y, fa)
persp(x, y, fa, theta=30) # theta: angles defining the viewing direction
persp(x, y, fa, theta=30, phi=20)  # theta and phi control the angles at which the plot is viewed



