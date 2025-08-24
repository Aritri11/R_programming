#performing all the exercises for heart.csv dataset
df=read.csv("C:/Users/Aritri Baidya/Downloads/Heart.csv")
print(dim(df))
print(colnames(df))
print(rownames(df))
print(head(df,30))
print(table(df$Age))
print(table(df$ChestPain))
print(table(df$Thal))
print(table(df$AHD))
#to know the categorical variable
print(unique(df$Sex))
print(unique(df$ChestPain))
print(unique(df$Thal))
print(unique(df$AHD))
#to know the levels of categorical variables
df$ChestPain=factor(df$ChestPain,levels=c("typical","asymptomatic", "nonanginal", "nontypical" ))
is.factor(df$ChestPain)
print(levels(df$ChestPain))
print(nlevels(df$ChestPain)) #levels=4
df$Thal=factor(df$Thal,levels=c('fixed','normal','reversable'))
is.factor(df$Thal)
print(levels(df$Thal))
print(nlevels(df$Thal)) #levels=3
df$AHD=factor(df$AHD,levels=c('Yes','No'))
is.factor(df$AHD)
print(levels(df$AHD))
print(nlevels(df$AHD)) #levels=2
#mean of RestBP
print(mean(df$RestBP))
#median of RestBP
print(median(df$RestBP))
#mode of RestBP
print(table(df$RestBP))
mode_val=names(sort(table(df$RestBP),decreasing=TRUE))[1]
print(mode_val)
#as mean>median>mode, so it is right-skewed and hence not symmetric
print(sd(df$RestBP)) #standard deviation of restbp column
print(summary(df$RestBP)) #statistical summary of restbp column
hist(df$RestBP) #histogram plot of restbp data and it is in accordance to previously calculated mean,median,mode
library(moments) #calling moments library to compute skewness and kurtosis
print(skewness(df$RestBP)) #skewness(right-skewed) of gtv column
print(kurtosis(df$RestBP)) #kurtosis(leptokurtic) of RestBP column

#for Chol column
boxplot(df$Chol) #simple box plot
boxplot(df$Chol, xlabel="Spread of Chol",ylabel="Cholestrol",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(df$Chol, xlabel="Spread of Chol",ylabel="Cholestrol",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(df$Chol, xlabel="Spread of Chol",ylabel="Cholestrol",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01
#range = 0.1: The whiskers will be closer to the box, and only very extreme values will appear as outliers.
#range = 0.2: The whiskers will extend farther out, and more data points will be captured within the whiskers.
#range = 0.01: The whiskers will be very short, and you will see a plot with only the most extreme outliers plotted as points.

#for RestBp column
boxplot(df$RestBP) #simple box plot
boxplot(df$RestBP, xlabel="Spread of BP",ylabel="BP",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(df$RestBP, xlabel="Spread of BP",ylabel="BP",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(df$RestBP, xlabel="Spread of BP",ylabel="BP",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01

#for MaxHR column
boxplot(df$MaxHR) #simple box plot
boxplot(df$MaxHR, xlabel="Spread of MaxHR",ylabel="MaxHR",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(df$MaxHR, xlabel="Spread of MaxHR",ylabel="MaxHR",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(df$MaxHR, xlabel="Spread of MaxHR",ylabel="MaxHR",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01
#MaxHR column has the broadest distribution
print(table(df$MaxHR))


#Ex4 
f1=subset(df,df$MaxHR>150) #MaxHR data with values greater than 20
print(f1)
f2=df[c(1,3,8,9,13,14, 18,21),] #all the mentioned rows are selected
print(f2)
f3_ind=which(df$ChestPain=='typical') #indices of rows containing typical
print(f3_ind)
f3=df[f3_ind,] #subset of the data containing typical
print(f3)
#extracting the MaxHR column
MaxHR_col=df$MaxHR 
print(MaxHR_col)
#extracting chol column
chol_col=df$Chol 
print(chol_col)
new_col=(MaxHR_col*chol_col)/234 #creating a new column with the formula:data$MaxHR*data$chol/234.
print(new_col)
#creating new dataframe with MaxHR,chol and new column values
new_df <- data.frame(MaxHR = df$MaxHR, Chol = df$Chol, New_Column = df$new_col)
print(new_df)
f=subset(df,df$MaxHR>150) #subset containing only MaxHR
print(f)
write.csv(f,file="C:/Users/Aritri Baidya/Downloads/lab5_new_heart_dataset.csv")
#Ex-5
df$ChestPain=factor(df$ChestPain,levels=c("typical","asymptomatic", "nonanginal", "nontypical" ))
is.factor(df$ChestPain)  #to check if it is a factor
print(levels(df$ChestPain)) #printing the levels
print(nlevels(df$ChestPain)) #levels=4
df$Thal=factor(df$Thal,levels=c('fixed','normal','reversable'))
is.factor(df$Thal)  #to check if it is a factor
print(levels(df$Thal)) #printing the levels
print(nlevels(df$Thal)) #levels=3
df$AHD=factor(df$AHD,levels=c('Yes','No'))
is.factor(df$AHD)  #to check if it is a factor
print(levels(df$AHD)) #printing the levels
print(nlevels(df$AHD)) #levels=2


#Ex6
temp=gl(3,10,303,labels=c('Hot','Cold','Lukewarm')) #creating a new column with labels=hot,cold and lukewarm
print(temp)
newdataframe=data.frame(df,temp) #adding the new column to the dataframe
print(newdataframe)
new=cbind(df,temp) #2nd method to do the above
print(new)

#Ex7
tapply(df$RestBP, df$Chol, mean) #to take out the mean of the 2 columns
tapply(df$RestBP, df$Chol, mean,trim=0.1) #trim helps in removing the outliers i.e when trim=0.1, 10% high and low values are discarded

#Ex8
print(pmin(df$RestBP,df$Chol,df$MaxHR)) #gives the minimum value along 3 parallel columns
print(pmax(df$RestBP,df$Chol,df$MaxHR)) #gives the maximum value along 3 parallel columns

#Ex9
ranks=rank(df$RestBP) #to get the rank of the column
sorted=sort(df$RestBP) #to sort the column
ordered=order(df$RestBP) #gives indices of sorted column
view=data.frame(df$RestBP,ranks,sorted,ordered) #adding the sorted,ordered and rank columns to the dataframe
print(view)
output=data.frame(df$Chol,df$RestBP,ordered) #adding the ordered column from previous along with diagnosis and gtv columns
print(output)
write.csv(output,file="/home/ibab/Downloads/lab4_ordered_data_BrainCancer.csv") #writing the subset to a new file

#Ex10
filter1=df[1:6,3:8] #extracting rows from 1-6 and columns from 3-8
filter1mat=as.matrix(filter1) #printing the above extracted rows and columns as a matrix
print(filter1mat)
print(class(filter1mat)) #to get the class
print(mode(filter1mat)) #to get the mode
print(attributes(filter1mat)) #to get the attributes
newcol=df$RestBP+df$Chol+df$MaxHR #creating a new column
newcoladded=data.frame(df,newcol) #adding the new column to the exsisting dataframe
print(newcoladded)
newcoladded2=cbind(df,newcol) #2nd method
print(newcoladded2)
filter4=df[c(26,35),] #creating a subset of 26th and 35th rows
newrowadded=rbind(df,filter4) #new row created using the above subset
print(newrowadded)
print(dim(newrowadded)) #printing the dimensions of the new dataframe
print(dim(df)) #the dimensions of original dataset
