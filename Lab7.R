################################# Lab 7 #########################################

#Exercise 1: Solving matrix equations and review of matrix operations.

# Creating the first matrix with byrow = TRUE
amat <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=TRUE)
print(amat)

# Creating the second matrix with byrow = FALSE
amat2 <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=FALSE)
print(amat2)
#The elements in amat are filled row by row, while the elements in amat2 are filled column by column.
#As a result, the two matrices have the same elements but arranged in a different order.

# Assigning row names and column names to the matrix 'amat'
rownames(amat) <- c("R1", "R2", "R3")
colnames(amat) <- c("C1", "C2", "C3", "C4")
print(amat)

# Creating Matrix A with the specified elements
A <- matrix(c(2, 5, 7, 3, 1, 8, 9, 10, 1, 12, 5, 10, 4, 17, 15, 11), nrow=4, ncol=4)
# Creating Matrix B with the specified elements
B <- matrix(c(12, 5, 3, 17, 1, 18, 9, 10, 1, 12, 5, 10, 4, 15, 15, 4), nrow=4, ncol=4)
print(A)
print(B)
# Element-wise multiplication of matrices A and B
result <- A * B
print(result)

# Define the vectors X and Y
X <- c(5, 6, 8, 9)
Y <- c(8, 10, 12, 5)
# Outer product of X and Y
outer_product <- outer(X, Y)
# Inner product (dot product) of X and Y
inner_product <- sum(X * Y)
print(outer_product)
print(inner_product)
# Create a diagonal matrix using the vector X
A <- diag(X)
print(A)
# Print the diagonal elements of A
print(diag(A))

# Create a 6x6 identity matrix in one line
identity_matrix <- diag(6)
# Print the identity matrix
print(identity_matrix)

# Create the matrix A with the given elements
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
print(A)
# Create the 3x1 matrix B with the given elements
B <- matrix(c(5, -3, 13), nrow=3, ncol=1)
print(B)

# Define matrix A
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
# Define matrix B (3x1 vector)
B <- matrix(c(5, -3, 13), nrow=3, ncol=1)
# Solve the equation AX = B to find X
X <- solve(A, B)
print(X)
print(class(X)) #X is a matrix
print(typeof(X))

# Define matrix A
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
print(A)
# Find the inverse of A
Ainv <- solve(A)
print(Ainv)
# Check if Ainv * A is the identity matrix
identity_check <- Ainv %*% A
print(identity_check) #gives an identity matrix

# Find eigenvalues and eigenvectors of A
results <- eigen(A)
# Print the eigenvalues and eigenvectors
print(results$values)
print(results$vectors)

#Perform matrix-vector multiplication of A and the second eigenvector
second_eigenvector <- results$vectors[,2]  # second eigenvector
A_times_second_eigenvector <- A %*% second_eigenvector
print(A_times_second_eigenvector)
# Check if the result is approximately the eigenvalue times the second eigenvector
eigenvalue_times_second_eigenvector <- results$values[2] * second_eigenvector
print(eigenvalue_times_second_eigenvector)
#A×second eigenvector gives a result approximately equal to the second eigenvalue multiplied by the second eigenvector


#Exercise 2: Removing a column from a data frame.

data=read.csv("/home/ibab/Downloads/BrainCancer.csv",header=TRUE)
print(colnames(data))
newcol=(data$gtv^2) + data$time
addcol=cbind(data,newcol)
print(addcol)
#Print the row and column names of the modified data
print(colnames(addcol))
print(rownames(addcol))
# Change row names using the paste() function
rownames(addcol) <- paste("Row-", 1:nrow(df), sep="")
print(addcol)
#Remove the column ’ki’ by assigning NULL value to this column. Print the modified data to make sure that the ‘ki’ column has indeed been removed
data$ki=NULL
print(colnames(data))


#Exercise 3: Reading excel files.

#working with xl files
library(readxl) #importing the library
d=read_excel('/home/ibab/Downloads/pone.0148733.s001.xlsx',1) #loading the dataset
#Print the column names and dimensions of the data in the data data frame
print(names(d))
print(dim(d))


#Exercise 4: Sets and operations on sets.

#Define a vector A with elements a,b,c,d,e. Also define another vector B with elements d,e,f,g. Print the vectors
A=c('a','b','c','d','e')
B=c('d','e','f')
print(A)
print(B)
#Perform a union operation between the two sets and print the result
union(A,B)
c(setdiff(A,B),intersect(A,B),setdiff(B,A)) #for union
# Perform an intersection operation between the two sets and print the result
intersect(A,B)
A[A%in%B] #another way
# Perform a difference operation between the two sets and print the result
setdiff(A,B)
setdiff(B,A)
#The function setequal() checks for the equality of two objects. Create an object with a sequence of A-B, A∩ B and B-A and use this function to check its contents with the result of the union operation above. Print the result.
setequal(c(setdiff(A,B),intersect(A,B),setdiff(B,A)),union(A,B))
#List the elements of B present in A using two different approaches
intersect(A,B)
B[B %in% A]
#Print the elements of A present in B
intersect(A,B)


#Exercise 5: Practice with subsets.

# Form a vector with elements
vector <- c(8, 10, 12, 7, 14, 16, 2, 4, 9, 19, 20, 3, 6)
# (a) Values greater than 12
filtered_a <- vector[vector > 12]
print(filtered_a)
# (b) Values greater than 10 and less than 20
filtered_b <- vector[vector > 10 & vector < 20]
print(filtered_b)
# Form an array with the given elements
A <- c(2, 7, 29, 32, 41, 11, 15, NA, NA, 55, 32, NA, 42, 109)
# Create a new array without NAs and keeping values less than 100
A_filtered <- A[!is.na(A) & A < 100]
print(A_filtered)
# Replace NA with 0 in array A
A_no_na <- ifelse(is.na(A), 0, A)
print(A_no_na)
# Create a vector of gene names
genes <- paste("gene", 1:7, sep="-")
# Create a vector for gender
gender <- c("M", "M", "F", "M", "F", "F", "M")
print(genes)
print(gender)
# Define 7 result vectors
result1 <- c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 <- c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 <- c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 <- c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 <- c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 <- c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 <- c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)
datframe=data.frame(genes = genes, gender = gender, result1 = result1, result2 = result2, result3 = result3, result4 = result4, result5 = result5, result6 = result6, result7 = result7)
print(datframe)
# Add new column names to the dataframe
colnames(datframe) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")
print(datframe)
# Create a subset where expt2 > 20
subset_expt2_gt_20 <- subset(datframe, expt2 > 20)
print(subset_expt2_gt_20)
# Create a subset where gender is Female
subset_female <- subset(datframe, Gender == "F")
print(subset_female)
# Create a subset where gender is Male and expt2 < 30
subset_male_expt2_lt_30 <- subset(datframe, Gender == "M" & expt2 < 30)
print(subset_male_expt2_lt_30)


#Exercise 6: If-else-if structure.

#Write an if-else-if structure that explores and prints the quadrant in which an angle belongs. For example, if you input 45 degree it should print ‘First quadrant’
angle=45
if (angle>=0 && angle<90){
  print('First quadrant')
}else if(angle>=90 && angle<180){
  print('Second quadrant')
}else if(angle>=180 && angle<270){
  print('Third quadrant')
}else if(angle>=270 && angle<360){
  print('Fourth quadrant')
} else{
  print('Invalid angle')
} 

#Write an if-else-if structure that takes three numeric inputs and uses this structure alone to put the 3 numbers in decreasing order. Do not use any built in function for sorting purposes
# Input three numbers
num1 <- 12
num2 <- 7
num3 <- 15
# Sorting numbers in decreasing order using if-else-if
if (num1 >= num2 & num1 >= num3) {
  if (num2 >= num3) {
    print(paste("The numbers in decreasing order: ", num1, num2, num3))
  } else {
    print(paste("The numbers in decreasing order: ", num1, num3, num2))
  }
} else if (num2 >= num1 & num2 >= num3) {
  if (num1 >= num3) {
    print(paste("The numbers in decreasing order: ", num2, num1, num3))
  } else {
    print(paste("The numbers in decreasing order: ", num2, num3, num1))
  }
} else {
  if (num1 >= num2) {
    print(paste("The numbers in decreasing order: ", num3, num1, num2))
  } else {
    print(paste("The numbers in decreasing order: ", num3, num2, num1))
  }
}
#Let’s say the cost of a journey ticket depends not only on the distance travelled but also on the details of the traveller. Distance-wise, the cost is a minimum of Rs.
#100 for the first 100km, Rs. 1.50 for every extra km until 1000km and Rs.2 per km thereafter. On top of that, senior citizens (> 60 years ) get a 25% concession
#and children under 6 years of age get 50% concession. Write a code that takes the journey distance and the traveller’s age as inputs, and prints out the ticket cost.
# Input: Journey distance and traveller's age
distance <- as.numeric(readline(prompt = "Enter the journey distance (in km): "))
age <- as.numeric(readline(prompt = "Enter the traveller's age: "))

# Initialize the ticket cost
ticket_cost <- 0

# Calculate the base cost based on the distance
if (distance <= 100) {
  ticket_cost <- 100  # Fixed cost for the first 100 km
} else if (distance <= 1000) {
  ticket_cost <- 100 + (distance - 100) * 1.50  # Rs. 1.50 per km after 100 km
} else {
  ticket_cost <- 100 + (1000 - 100) * 1.50 + (distance - 1000) * 2  # Rs. 2 per km after 1000 km
}

# Apply concessions based on age
if (age > 60) {
  ticket_cost <- ticket_cost * 0.75  # 25% concession for senior citizens
} else if (age < 6) {
  ticket_cost <- ticket_cost * 0.50  # 50% concession for children under 6 years
}

# Print the final ticket cost
print(paste("The total ticket cost is: Rs.", round(ticket_cost, 2)))


#Exercise 7: Writing functions.

#Write a function to replace all the negative values in a vector by zeros
replace_negatives <- function(vec) {
  vec[vec < 0] <- 0
  return(vec)
}
replace_negatives(c(-1,3,-5,-8,6))

#Write a function to calculate the factorial of a number using the Stirling’s approximation
stirling_factorial <- function(n){
  if (n < 0){
    print("Factorial is not defined for negative numbers.")
  }
  sqrt(2 * pi * n) * (n / exp(1))^n * (1 + 1 / (12 * n) + 1 / (288 * n^2) - 139 / (51840 * n^3) - 571 / (2488320 * n^4))
}
stirling_factorial(6)

#Write a function to sum the digits of a number
sum_digits <- function(num) {
  digits <- as.numeric(unlist(strsplit(as.character(num), "")))
  return(sum(digits))
}
sum_digits(123)



















