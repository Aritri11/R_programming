######################################################Lab1#############################################################

#Exercise 1: Arithmetic operations and operator precedence

print(2.7/2)
#'/' gives the quotient
2.7/2
#'%/%' integer quotient
2.7%/%2
#'%%' gives remainder
2.7%%2
#dividing imaginary number
10+5i/2
#rounds the number to nearest integer 
round(2.5)
round(-2.5)
#modulus function
2%/%4-1
#power
3*2**2
#power
3*2^2
#power
3**2*2
#integer quotient
7%/%4
#modulus
7%%4
#-7 modulo 4
-7%%4
#truncates decimal part and returns integer part
trunc(5.7)
#truncates decimal part and returns integer part
trunc(-5.7)


#Exercise 2: We know the result of ceiling(5.7), which is 6. Write a one-line function to do the same.

ceiling_func=function(n){ floor(n+0.5)}
ceiling_func(5.7)  


#Exercise 3: Logical operations

a=1
b=2
c=4
print(a&b) #returns TRUE as both a and b are non-zero
print(!a<b | c>b ) # (!a<b) is FALSE and c>b is TRUE therefore AND(|) gives TRUE


#Exercise 4: Intricacies of the integer function

x<-c(5,3,7,8)
#default datatype for number vector is numeric and not integer
is.integer(x)
is.numeric(x)
x<-integer(x)
print(x)
#converts the numeric datatype to integer datatype
x<-as.integer(x)
#checks if it is integer datatype
is.integer(x)

#Exercise 5: Testing equality of two real numbers

x<-sqrt(2) #assigns the square root value of 2 to x
x*x==2 #returns FALSE because x*x is slightly less than 2 and not exactly 2
x*x-2 #returns the total rounding error
all.equal(x*x,2) #'all.equal' specifically used for compairing real numbers