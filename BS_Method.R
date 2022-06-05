###Bisection Method###
#Version: 1.0
#Date: June, 5th 2022

#Applicability: This method is used to find the roots of a polynomial equation.

#Libraries
library(tidyverse)

##Approach 1 - Maximum number of iterations 

#Steps:
#Step 1 - Create a Function
fx = function(x){
  value<- (x*x*x*x) - x - 10
  return(value)
}

#Step 2 - Set Input Parameters
print("Enter a limit for a and b")
print("Enter the lower limit: a")
a<- as.double(readline(prompt = "a: ")) 
print("Enter the upper limit: b")
b<- as.double(readline(prompt = "b: ")) 
print("Enter the number of iterations")
n<- as.integer(readline(prompt = "n: "))

#Step 3 - Parameters Validation
va = if_else(fx(a) * fx(b) <0, print("ok"), print("try new values")) 
va

#Step 4 - Loop
i<-0 #seed

while(i < n){
  c = (a+b)/2  
  
  if (fx(a) * fx(c) < 0){
    b <- c
  }else{
    a <- c
  }
  
  #increment
  i<-i+1
  cat("At iteration", i, "whose value of c is", c, "and f(c) is", fx(c) , "\n")  
  
}

##Approach 2 - Maximum tolerance
