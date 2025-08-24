#################################### Lab 2 ############################################

#Exercise 1: An R quirk

round(123456789,digits = 3) 
round(12.1343,digits = 3)
round(123.12344,digits=3)
round(1234.12344,digits=3)
round(12345.12344,digits=3)
options(digits=15)
c=12345.123456789
print(c)
options(digits = 5)
c=12345.123456789
print(c)
options(digits=10)
c=12345.123456789
print(c)
options(digits=15)
print(round(123456789,digits = 3))
print(round(12.1343,digits = 3))
print(round(123.12344,digits=3))
print(round(1234.12344,digits=3))
print(round(12345.12344,digits=3))
formatC(round(12345.12344,digits=3),format="f",digits=3)
print(1234.12344)
print(1234.723,digits=3)
print(1234.723,digits=5)
round(123456788.123,digits=3)
print(round(123456789.1234,digits=4),digits=20)
paste("Hello World")
paste ("Hello","World")
paste (1:10)
paste(11:20) [4]
as.numeric((paste (1:10)))
paste(1:10,collapse = ".")
paste(c("Hello","World"),1:10,sep="_")
paste("Aritri","Baidya",1,2,3,sep="+")
print(paste("Aritri","Baidya",1,2,3,sep="."))
paste(c("Aritri","Baidya",1,2,3),collapse="+")
paste(c("Hello","World"),1:10,sep="-")
print(paste("Hello",1:10,sep="-"))
print(paste("Hello","World",1:10,sep="-"))
print(paste(c("Aritri","Baidya"),1:10,collapse=".."))
print(paste("Aritri","ab"),collapse="")


#Exercise 2: Generating sequences

0:10 #generates sequence from 0 to 10
15:5 #generates sequence from 15 to 5 in reverse order
seq(0,1.5,0.1)  #generates sequence from 0 to 1.5 by a step of 0.1
seq(6,4,-0.2) #generates sequence from 6 to 4 by a step of 0.2 in reverse order
N=c(55,76,92,103,84,88,121,91,65,77,99)#generates a sequence
seq(0.04,by=0.01,along=N)#generates sequence from 0.04 to the length of N by 0.01 steps
seq(from=0.04,to=0.14,along=N) #generates sequence from 0.04 to the length of N by 0.01 steps
sequence(c(4,3,4,4,4,5))#generates a sequence containing 1-4,1-3,1-4,1-4,1-4 and 1-5
rep(9,5) #replicates 9 only 5 times
rep(1:4,2) #replicates 1-4 only 2 times
rep(1:4,each=2) #replicates 1-4 in which each number is repeated 2 times
rep(1:4,each=2,times=3) #replicates 1-4 in which each number is repeated 2 times and whole is repeated 3 times
rep(1:4,c(4,1,4,2)) #repeats 1 4 times 2 1 time etc...
rep(c("cat","dog","goldfish","rat"), c(2,3,2,1)) #repeats each element in 1st list to numbers of times in the 2nd list
seq(-1,1,0.1) # generates sequence from -1 to 1 in a step of 0.1  
seq(-1,1,length=7) #generates sequence from -1 to 1 having length of 7
number<--1+0.1*(0:20)
print(number)# generates sequence from -1 to 1 in a step of 0.1  

#Exercise 3: Missing values, infinity and NaN, NA

3/0
exp(-Inf)
(0:3)**Inf
0/0
Inf - Inf
Inf/Inf
is.finite(10)
is.infinite(10)
is.infinite(Inf)
y<- c(4,NA,7)
y=="NA"
is.na(y)
y[!is.na(y)]
c1=c(1,2,3,NA)
c2=c(5,6,NA,8)
c3=c(9,NA,11,12)
c4=c(NA,14,15,16)
full.frame <- data.frame (c1,c2,c3,c4)
full.frame
full.frame[,]
full.frame[1,]
full.frame[c1,]
full.frame[c2,]
full.frame[c3,]
full.frame[c(1,3),]
full.frame[c(1,2,3)]
reduced.frame=full.frame[! is.na(full.frame$c1),]
reduced.frame
reduced.frame=full.frame[! is.na(full.frame$c1),2]
reduced.frame
full.frame[! is.na(full.frame)]
full.frame[! is.na(full.frame),]
p1=c(1,2,3,4)
p2=c(6,7,8,9)
p3=c(7,8,1,2)
p4=c(1,1,1,1)
full.frame=data.frame(p1,p2,p3,p4)
full.frame
full.frame[p2,2]
mean(x,na.rm=T)
v <- c(1:6,NA,NA,9:12)
print(v)
seq(along=v)[is.na(v)]
which(is.na(v))


#Exercise 4: Vectors

vec=c(90,5,8,9,10,45,55) #creating a vector
class(vec) #class of the vector which is "numeric
vec1=c(3,7,4,10,"Aritri") #creating another vector
class(vec1) #now the class is not numeric as there is a string element in the vector
max(vec) #to find the maximum value in the vector
min(vec) #to find the minimum value in the vector
vec2=scan() #to take a series of entries
vec[4] #to print the 4th element in the vector
ind=c(2,3,4) #to define a group of indices  
vec[ind] #to get the values of only the defined indices
vec[c(2,3,8)] #another way of writing the above
vec[length(vec)] #gives the length of the vector
vec[-1] #removes the first element in the vector vec
vec_red=vec[-1] #removes the first element from vector vec and assigned it to another variable
length(vec_red) #get the length of vec after removing first element
vec[-length(vec)] #removes the last element
vec[c(-2,-3)] #removes the second and the third element from vec
vec[-2:-4] #removes elements between indices 2-4
#function to largest two and smallest two elements from a vector
trim=function(x){
  vec_sort=sort(x)
  print(vec_sort)
  trimmed_x=vec_sort[3:(length(vec_sort)-2)]
  print(trimmed_x)
}
trim(vec)
#another way to do the above
trim1=function(x) sort(x) [c(-1,-2,-(length(x)-1),-length(x))]
trim1(vec)

vec[seq(2,length(vec),2)] #elements of even indices
vec[1:length(vec)%%2==0] #another way to do the above
x=1:10 #stores 1 to 10 in x
x
x[x<5] #prints only the elements less than 5
sum(x[x<5])
#function to add the 3 largest elements of any given vector
add=function(x){
  s=sort(x) 
  ss=x[c((length(x)-2),length(x)-1,length(x))]
  r=sum(ss)
  print(r)
} 
add(vec)
which.max(x) #gives the index of largest element
which.min(x) #gives the index of smallest element
cbind(1:10,10:1) #column bind
rbind(1:10,10:1) #row bind
X <- c(1:10) #stores values 1 to 10
X
Y <- c(1:10*5) #stores values of multiples of 5 upto 10
Y
X*Y
X+Y
X/Y
X^Y
log(X)
exp(Y)

#Exercise 5: Matrices/Dataframes/Arrays

y=1:24
dim(y)=c(2,4,3) #forming a 3D array
y
X <- matrix (c(1,0,0,0,1,0,0,0,1),nrow=3) #forming a matrix with 3 rows
X
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=T,nrow=2) #method 1 of converting vector to matrix by rows
V
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=F,nrow=2) #method 1 of converting vector to matrix by columns
V
dim(vector) <- c(4,2) #method 2 of converting vector to matrix by columns
vector
is.matrix(vector) #true
#numeric and string datas combined
x=c(1,2,3,4,"AB","R")
dim(x)=c(3,2,1)
x
#4D array
vec=1:64
dim(vec)=c(2,2,4,4)
vec
vec[1,1,2,4]
vec[,,1,1]

#Exercise 6: Vector functions

X=matrix(c(1:24),nrow=3) #creating a matrix with 3 rows
X
Y=(1:10*5) #matrix with 1 row
Y
colMeans(X) #to calculate the mean of each column
Z <- X[1:4] %o% Y[1:3] #calculate the outer product
Z
YoX <- Y[1:3] %o% X[1:4]#calculate the outer product
YoX
t(Z) #transpose of Z
t(t(Z)) #transpose of transpose results in original matrix
t(YoX)
X=matrix(c(1:24),nrow=3)
Y=(1:8*5)
X %*% Y #dot product
sum(X*Y) #another way to carry out dot product
crossprod(X[1:4],Z)
diag(4) #identity matrix
class(X)#matrix
attributes(X) #dimensions of the matrix
X=matrix(c(1:24),nrow=3)
Y=(1:8*5)
X[2,3]=NA #substituting a value with NA
X
X %*% Y #anything multiplied with NA is still NA


A=matrix(c(1:50),nrow=5)
A
B=matrix(c(60:90),nrow=4)
B

