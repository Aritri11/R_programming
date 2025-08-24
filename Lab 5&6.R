################################### Lab 5&6 ########################################


#Exercise1: Loading the dataset(BrainCancer)
df=read.csv("/home/ibab/Downloads/BrainCancer.csv")

#Exercise2: Print the basic features of the data

print(dim(df)) #dimensions of the data
print(colnames(df)) #column names of the data
print(rownames(df)) #row names of the data
print(head(df,30)) #first 10 lines of the dataset
#data representation as a frequency table
print(table(df$diagnosis))
print(table(df$gtv))
print(table(df$loc))
print(table(df$ki))
#count of categorical variables is:4 and they are listed below:
print(unique(df$sex))
print(unique(df$dignosis))
print(unique(df$loc))
print(unique(df$stereo))
#defining categorical variables:
df$sex=factor(df$sex,levels=c('Male','Female')) 
is.factor(df$sex)
print(levels(df$sex))
print(nlevels(df$sex)) #level=2

df$diagnosis=factor(df$diagnosis,levels=c( "Meningioma", "HG glioma",  "LG glioma" ,"Other" ))
is.factor(df$diagnosis)
print(levels(df$diagnosis))
print(nlevels(df$diagnosis)) #level=4

df$loc=factor(df$loc,levels=c( "Infratentorial", "Supratentorial"))
is.factor(df$loc)
print(levels(df$loc))
print(nlevels(df$loc)) #level=2

df$stereo=factor(df$stereo,levels=c(  "SRS" ,"SRT"))
is.factor(df$stereo)
print(levels(df$stereo))
print(nlevels(df$stereo)) #level=2


#Exercise 3: Print the following statistical parameters of the data of

print(mean(df$gtv)) #mean of column gtv
print(mean(df$time)) #mean of column time
print(median(df$gtv)) #median of column gtv
#the mean is greater than the median, the distribution of the data is likely right-skewed (positively skewed), meaning there are some higher values pulling the mean upwards.
print(table(df$gtv))
#mode of gtv column:
mode_value <- names(sort(table(df$gtv), decreasing = TRUE))[1] #forms a table ,then sorts the values in descending order and then returns the actual name of the field with max frequency
print(mode_value)
#as mean>median>mode, so it is right-skewed and hence not symmetric
print(sd(df$gtv)) #standard deviation of gtv column
print(summary(df$gtv)) #statistical summary of gtv column
hist(df$gtv) #histogram plot of gtv data and it is in accordance to previously calculated mean,median,mode
library(moments) #calling moments library to compute skewness and kurtosis
print(skewness(data$gtv)) #skewness(right-skewed) of gtv column
print(kurtosis(data$gtv)) #kurtosis(leptokurtic) of gtv column

#for gtv column
boxplot(data$gtv) #simple box plot
boxplot(data$gtv, xlabel="Spread of GTV",ylabel="GTV",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(data$gtv, xlabel="Spread of GTV",ylabel="GTV",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(data$gtv, xlabel="Spread of GTV",ylabel="GTV",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01
#range = 0.1: The whiskers will be closer to the box, and only very extreme values will appear as outliers.
#range = 0.2: The whiskers will extend farther out, and more data points will be captured within the whiskers.
#range = 0.01: The whiskers will be very short, and you will see a plot with only the most extreme outliers plotted as points.

#for ki column
boxplot(data$ki) #simple box plot
boxplot(data$ki, xlabel="Spread of ki",ylabel="Ki",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(data$ki, xlabel="Spread of ki",ylabel="Ki",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(data$ki, xlabel="Spread of Ki",ylabel="Ki",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01

#for time column
boxplot(data$time) #simple box plot
boxplot(data$time, xlabel="Spread of time",ylabel="time",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(data$time, xlabel="Spread of time",ylabel="time",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(data$time, xlabel="Spread of time",ylabel="time",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01
#time column has the broadest distribution


#Exercise 4: Build subsets of the data below.

f1=subset(df,df$gtv>20) #gtv data with values greater than 20
print(f1)
f2=df[c(1,3,8,9,13,14, 18,21),] #all the mentioned rows are selected
print(f2)
f3_ind=which(df$sex=='Female') #indices of rows containing female
print(f3_ind)
f3=df[f3_ind,] #subset of the data containing female
print(f3)
#extracting the gtv column
gtv_col=df$gtv 
print(gtv_col)
#extracting ki column
ki_col=df$ki 
print(ki_col)
new_col=(gtv_col*ki_col)/234 #creating a new column with the formula:data$gtv*data$ki/234.
print(new_col)
#creating new dataframe with gtv,ki and new column values
new_df <- data.frame(GTV = df$gtv, KI = df$ki, New_Column = df$new_col)
print(new_df)
f=subset(df,df$sex=='Female') #subset containing only females
print(f)
write.csv(f,file="/home/ibab/Downloads/lab4_female_BrainCancer.csv") #writing the subset to a new file


#Exercise 5: Manipulating the ‘factors’ in existing data

df$sex=factor(df$sex,levels=c('Male','Female'))
print(is.factor(df$sex)) #to check if it is a factor
df$diagnosis=factor(df$diagnosis,levels=c('Meningioma','HG glioma','LG glioma','other'))
print(is.factor(df$diagnosis)) #to check if it is a factor
df$sex=factor(df$loc,levels=c('Infratentorial','Supratentorial'))
print(is.factor(df$loc)) #to check if it is a factor
print(levels(df$sex)) #printing the levels
print(nlevels(df$sex)) #printing the number of levels
print(levels(df$diagnosis)) #printing the levels
print(nlevels(df$diagnosis)) #printing the number of levels

#Exercise 6: Generating factor levels using gl() function

temp=gl(3,2,88,labels=c('Hot','Cold','Lukewarm')) #creating a new column with labels=hot,cold and lukewarm
print(temp)
newdataframe=data.frame(df,temp) #adding the new column to the dataframe
print(newdataframe)
new=cbind(df,temp) #2nd method to do the above
print(new)

#Exercise 7: Using the tapply() function

tapply(df$gtv, df$ki, mean) #to take out the mean of the 2 columns
tapply(df$gtv, df$ki, mean,trim=0.1) #trim helps in removing the outliers i.e when trim=0.1, 10% high and low values are discarded


#Exercise 8: Finding parallel-max and min.

print(pmin(df$gtv,df$time,df$ki)) #gives the minimum value along 3 parallel columns
print(pmax(df$gtv,df$time,df$ki)) #gives the maximum value along 3 parallel columns

#Exercise 9: Difference between rank(), sort() and order().

ranks=rank(df$gtv) #to get the rank of the column
sorted=sort(df$gtv) #to sort the column
ordered=order(df$gtv) #gives indices of sorted column
view=data.frame(df$gtv,ranks,sorted,ordered) #adding the sorted,ordered and rank columns to the dataframe
print(view)
output=data.frame(df$diagnosis,df$gtv,ordered) #adding the ordered column from previous along with diagnosis and gtv columns
print(output)
write.csv(output,file="/home/ibab/Downloads/lab4_ordered_data_BrainCancer.csv") #writing the subset to a new file


#Exercise 10: Converting data frames to matrices.

filter1=df[1:6,3:8] #extracting rows from 1-6 and columns from 3-8
filter1mat=as.matrix(filter1) #printing the above extracted rows and columns as a matrix
print(filter1mat)
print(class(filter1mat)) #to get the class
print(mode(filter1mat)) #to get the mode
print(attributes(filter1mat)) #to get the attributes
newcol=df$ki+df$gtv+df$time #creating a new column
newcoladded=data.frame(df,newcol) #adding the new column to the exsisting dataframe
print(newcoladded)
newcoladded2=cbind(df,newcol) #2nd method
print(newcoladded2)
filter4=df[c(26,35),] #creating a subset of 26th and 35th rows
newrowadded=rbind(df,filter4) #new row created using the above subset
print(newrowadded)
print(dim(newrowadded)) #printing the dimensions of the new dataframe
print(dim(df)) #the dimensions of original dataset

#Exercise 11: Adding row and column names.

X=matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print(X)
print(rownames(X))
print(colnames(X))
rownames(X)=rownames(X,do.NULL=FALSE,prefix='Trial')
print(rownames((X)))
print(X)
drugs=c('aspirin','paracetamol','nurofen','palacebo','hedex')
colnames(X)=drugs
print(colnames(X))
print(X)
dimnames(X)=list(NULL,paste("drug",1:5,sep="")) #Changes in column names
print(X)

#Exercise 12: Calculations on rows or columns of the matrix.

print(mean(X[,5]))
print(var(X[4,]))
print(rowSums(X)) #calculates the sum of elements along each row
print(colSums(X)) #calculates the sum of elements along each column
print(apply(X,1,sum))
print(apply(X,2,sum))
print(rowMeans(X))
print(colMeans(X))
print(apply(X,1,mean))
print(apply(X,2,mean))
print(apply(X,2,sqrt))
print(apply(X,2,function(X)X^2+X))
group=c("A","B","B","A")
print(rowsum(X,group)) #Sum groups of rows within a column
print(row(X))
print(col(X))
print(tapply(X,list(group[row(X)], col(X)),sum))
print(aggregate(X,list(group),sum))
print(apply(X,2,sample))
X=rbind(X,apply(X,2,mean))
print(X)
X=cbind(X,apply(X,1,var))
print(X)
heading=c(paste('drug',1:5,sep=''),'var')
dimnames(X)=list(NULL,heading)
print(X)
rowhead=c(paste('Trial',1:4,sep=''),mean)
dimnames(X)=list(rowhead,heading)
print(X)


#Exercise 13: The sweep() function is used to ‘sweep out’ summaries from vectors, matrices, arrays or dataframes.

eg_sweep=data.frame(data$ki,data$gtv,data$time)
#method 1 to perform sweep action
cols=apply(eg_sweep,2,mean)
print(cols)
col.means=matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),nrow=dim(eg_sweep)[1])
print(col.means)
eg_sweep_alt=eg_sweep-col.means
print(eg_sweep_alt)
eg_sweep_alt2=sweep(eg_sweep,2,cols)
print(eg_sweep_alt2)
#sapply for vectors
eg_sapply=sapply(3:7,seq)
print(attributes(eg_sapply))
print(class(eg_sapply))


#Exercise 14: Using the max.col function. Read the data present in pgfull.txt. Extract columns
# 1:54 from the read data and call the subset data as species. max.col(species) returns
# the column index of the max value in each row; print this. We will use these
# indices to collect the names of the species as names(species)[max.col(species)].
# We then use the table function to build a frequency table of each of the species as
# table(names(species)[max.col(species)]. It turns out that there is no min.col()
# function in R, and one needs an alternative. How can max.col be used to extract the
# column indices that give the minimum value along each row?

pgdata=read.table("/home/ibab/Downloads/pgfull.txt")
print(pgdata)
print(names(pgdata))
species=as.matrix(pgdata[,1:54])
species[is.na(species)] <- 0
print(max.col(species))
print(names(species)[max.col(species)])
print(table(names(species)[max.col(species)]))
print(max.col(-species))

#Exercise 15: Introduction to Lists

apples <- c(4,4.5,4.2, 5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
data.frame(apples,oranges,chalk) #different rows so data frame is not possible
#subscripts on lists have double square brackets
print(items[[3]])
print(items[[3]][3])
print(items[3])
print(items[[1]][2])
print(items[1][2])

#Ex-15(ii)
items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(names(items))
print(items$fourth)
print(class(items))
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean)) #Predict the output first
print(summary((items)))
print(str(items))
#difference between class and mode

