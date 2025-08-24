################################ Lab8 ######################################### 


#Exercise1:-Given an integer, write a function to find if the integer is a palindrome.

is_palindrome <- function(num) {
  num_str <- as.character(num)  # Convert the integer to a string
  return(num_str == paste(rev(strsplit(num_str, NULL)[[1]]), collapse = ""))
}
is_palindrome(12321)
is_palindrome(12345)


#Exercise2:-Slice the string ‘seemerightnow’ to produce the following substrings: (a) ‘see’ (b) ‘me’(c) ‘right’

string <- "seemerightnow"
substring1 <- substr(string, 1, 3)
substring2 <- substr(string, 4, 5)  
substring3 <- substr(string, 6, 10)  
print(substring1)
print(substring2)
print(substring3)

#Exercise3:-Determine the fraction of G and C bases in the sequence “ATTGCGCATAGTCCGGG”.

library(stringr)
sequence <- "ATTGCGCATAGTCCGGG"
gc_fraction <- function(seq) {
  gc_count <- str_count(seq,pattern="GC")
  return(gc_count / nchar(seq))
}
gc_fraction(sequence)

#Exercise4:-Write a function to determine if a DNA nucleotide sequence is a palindrome in the
#sense that it is equal to its own complementary sequence read backward. For example,
#the sequence “TGGATCCA” is palindromic because its complement is “ACCTAGGT”
#which is same as the original sequence backward. The complementary base pairs are (A,T) and (G,C).

complement <- function(seq) {
  complement_map <- c(A = "T", T = "A", G = "C", C = "G")
  return(paste(complement_map[strsplit(seq, NULL)[[1]]], collapse = ""))
}
is_dna_palindrome <- function(seq) {
  rev_complement <- complement(seq)
  return(rev_complement == paste(rev(strsplit(seq, NULL)[[1]]), collapse = ""))
}
is_dna_palindrome("TGGATCCA")  
is_dna_palindrome("ATTGCGCATAGT")

#Exercise5:-Write a code to search and print the largest word in this sentence: ”She sells hundreds
#of sea oysters on the sea shore.” By extension, print the second largest word in the same
#sentence. Extend this code to allow searching for words of equal length and gathering them into a list.

find_largest_words <- function(sentence) {
  words <- unlist(strsplit(sentence, " "))
  word_lengths <- nchar(words)
  max_length <- max(word_lengths)
  second_max_length <- max(word_lengths[word_lengths < max_length])
  largest_words <- words[word_lengths == max_length]
  second_largest_words <- words[word_lengths == second_max_length]
  return(list(largest_words = largest_words, second_largest_words = second_largest_words))
}
sentence <- "She sells hundreds of sea oysters on the sea shore."
result <- find_largest_words(sentence)
print(result)


#Exercise6:-Load the data in ‘worldfloras.txt’ and do the following.

#(a) Create subsets of countries within the same continent and store the data (ie. the allied columns) as different dataframes.
# Load the data
world_data <- read.table("/home/ibab/Downloads/worldfloras.txt", header = TRUE, sep = "\t")
# Check the structure of the data
str(world_data)
# Create subsets for each continent (assuming the continent is named "Continent" and country is "Country")
asia_data <- subset(world_data, Continent == "Asia")
europe_data <- subset(world_data, Continent == "Europe")
africa_data <- subset(world_data, Continent == "Africa")
america_data <- subset(world_data, Continent == "America")
oceania_data <- subset(world_data, Continent == "Oceania")
# View a few rows of one of the dataframes as a sample
head(asia_data)


#(b) Make a boxplot of the distribution of floral count within each continent and print the statistical summary. What are the mean and standard deviation values? Also calculate and comment on the skewness and kurtosis parameters (interpret them)
# Boxplot for floral count distribution within each continent
boxplot(Flora ~ Continent, data = world_data, main = "Floral Count Distribution by Continent", xlab = "Continent", ylab = "Floral Count", col = "lightgreen", border = "black")
summary_stats <- aggregate(Flora ~ Continent, data = world_data, summary)
print(summary_stats)
mean_floral <- mean(world_data$Flora)
sd_floral <- sd(world_data$Flora)
print(mean_floral)
print(sd_floral)
library(e1071)
floral_skewness <- skewness(world_data$Flora)
floral_kurtosis <- kurtosis(world_data$Flora)
print(floral_skewness) #skewness is 3.835473 which indicates that the data has a long tail to the right
print(floral_kurtosis) #kurtosis is 19.1006 which indicates a more peaked distribution than a normal distribution (leptokurtic)


#(c) Make a boxplot and histogram plot of the population distribution within each con-tinent and print the statistical summary. Calculate and comment on the skewnessand kurtosis parameters (interpret them). Does this have any relation with the floral count data?
boxplot(Population ~ Continent, data = world_data, main = "Population Distribution by Continent", xlab = "Continent", ylab = "Population", col = "lightgreen", border = "black")
hist(world_data$Population, main = "Population Distribution", xlab = "Population", col = "purple", border = "black")
population_stats <- aggregate(Population ~ Continent, data = world_data, summary)
print(population_stats)
population_skewness <- skewness(world_data$Population)
population_kurtosis <- kurtosis(world_data$Population)
print(population_skewness)
print(population_kurtosis)
#Population distributions tend to be skewed right because of the small number of countries with extremely high populations.
# Population distributions can have heavy tails (high kurtosis), meaning a few countries with very large populations may significantly affect the distribution.


#Exercise7: Read in the data from ‘HumanBones.txt’ and group the data into categories “Chest”,“Spine”,“Skull”, “Ear Bones”, “Arms” and “Legs”. The number in the brackets indicates the number of bones in that type. Create a dataframe with 3 columns- category, name of the bone and number of bones.

bones_data <- read.table("/home/ibab/Downloads/HumanBones.txt", header = FALSE,sep = "\t", stringsAsFactors = FALSE)
head(bones_data)

categories <- c()
bone_names <- c()
bone_numbers <- c()
current_category <- NULL
for (i in 1:nrow(bones_data)) {
  line <- bones_data$V1[i]
  if (!grepl("\\(", line)) {
    current_category <- line
  } else {
    bone_info <- strsplit(line, "\\(")[[1]]
    bone_name <- trimws(bone_info[1])  # Get the bone name
    bone_number <- gsub("[^0-9]", "", bone_info[2])  # Extract the number of bones
    categories <- c(categories, current_category)
    bone_names <- c(bone_names, bone_name)
    bone_numbers <- c(bone_numbers, as.numeric(bone_number))
  }
}
bones_info <- data.frame(category = categories, name_of_bone = bone_names, number_of_bones = bone_numbers, stringsAsFactors = FALSE)
head(bones_info)


#Exercise8:-Which category contains maximum number of bones? Create a frequency table and make a bar plot of each category.

category_bones_summary <- aggregate(number_of_bones ~ category, data = bones_info, sum)
max_category <- category_bones_summary[which.max(category_bones_summary$number_of_bones), ]
print(max_category$category) #arms category
category_frequency <- table(bones_info$category)
print(category_frequency)
barplot(category_bones_summary$number_of_bones, names.arg = category_bones_summary$category, main = "Number of Bones by Category", xlab = "Category", ylab = "Number of Bones", col = "pink")


#Exercise9:-Create a subset category of “Legs” bones and print the bone names longer than 5 letters.

legs_data <- subset(bones_info, category == "Legs")
long_bones_legs <- subset(legs_data, nchar(name_of_bone) > 5)
print(long_bones_legs$name_of_bone)

#Exercise10:-List all the bones starting with “M” and substitute the lower-case “a” with upper-case“A”.

bones_starting_M <- subset(bones_info, grepl("^M", name_of_bone))
bones_starting_M$name_of_bone <- gsub("a", "A", bones_starting_M$name_of_bone)
print(bones_starting_M$name_of_bone)


#Exercise11:-List all the bones ending with “e” and convert all the letters to lower-case.

bones_ending_with_e <- subset(bones_info, grepl("e$", name_of_bone))
bones_ending_with_e$name_of_bone <- tolower(bones_ending_with_e$name_of_bone)
print(bones_ending_with_e$name_of_bone)

#Exercise12:-List all the bones with two “o” s in their names.

bones_with_two_o <- subset(bones_info, grepl("o.*o", name_of_bone))
print(bones_with_two_o$name_of_bone)

