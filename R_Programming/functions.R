#Define a function using "function(input1, ...,  inputn){..., return value}"
TestFunction = function(x, y){
  SumAmount = x + y
  ProductPrice = x * y
  c(SumAmount, ProductPrice)
}

x =TestFunction(2, 4)
x
x[1]
x[-2]


Distance = function(a, b){
  Dist = sqrt(sum((a-b)^2))
  Dist
}

x1 = c(1,2,3,4)
x2 = c(0,0,1,2)
y = Distance(x1, x2)
y

# write a function that takes a vector as an input and return mean and sd
FunctionMeanSD = function(a){
  a1 = mean(a)
  a2 = sd(a)
  c(a1, a2)
}
a = FunctionMeanSD(x1)
a
