################################ Lab 14 ##########################################


# I. Error bars, covariance and correlation
# An error is a small line segment around each point (X,Y) representing the uncertainity in the
# measured value Y. The bar might be any one of the following: (a) standard deviation sy, (b)
# standard error of the mean σy/√n orsy/√n and (c) confidence interval around the (unknown)
# population mean given by Z1−α/2σy/√n or if the population variance is not known, T1−α/2sy/√n.
# 
# For this we will use the arrows() function:
#   arrows(x0, y0, x1, y1, length, angle, code = 2, col, lty, lwd ) where the arrow is
# drawn from (x0,y0) to (x1,y1), with given length, angle between the arrow head and arrow
# head, and code to specify whether arrow heads at one end or both.
# (1) Error bars on bar plots: Enter the following data into R:
#   means = c(20.34,19.49,25.68)
# stderr = c(0.83,1.51,1.39)
# Make a barplot with the following features: the three means should be labeled as ‘A’,
# ‘B’ and ‘C’, grey filled bars, plot title as ’Errors on bar plot’. Use the arrows() function
# to plot the error bars as follows:
#   arrows(<barplotobject>, means+stderr, <barplotobject>,means-stderr,
#          angle=90,code=3,length=0.06,col=’red’)


# Given data
means <- c(20.34, 19.49, 25.68)
stderr <- c(0.83, 1.51, 1.39)

# Create a barplot with grey bars and labels A, B, C
bar_positions <- barplot(
  means,
  names.arg = c("A", "B", "C"),
  col = "grey",
  ylim = c(0, max(means + stderr) + 2),  # Add some space for error bars
  main = "Errors on bar plot",
  ylab = "Mean Value"
)

# Add error bars using arrows()
arrows(
  x0 = bar_positions, y0 = means + stderr,
  x1 = bar_positions, y1 = means - stderr,
  angle = 90, code = 3, length = 0.06, col = "red"
)
box()



# (2) Error bars on (x,y) plots: Enter the following data into R. The errors provided are
# standard errors on the mean
# x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
# y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
# errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)
# Plot (x,y) with points and xlabel ‘concentration’ and ylabel ‘optical activity’ and plot title
# as ‘Error bars on data points”. Again we can use the arrows() function to plot the error
# bars, only difference is in the first 4 arguments: arrows(x,y+errors,x,y-errors,.....)


# Clean start: redefine all
x <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y <- c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors <- c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

# Plot the points
plot(
  x, y,
  pch = 16,
  xlab = "concentration",
  ylab = "optical activity",
  main = "Error bars on data points",
  ylim = c(min(y - errors), max(y + errors))
)

# Add error bars
arrows(
  x0 = x, y0 = y + errors,
  x1 = x, y1 = y - errors,
  angle = 90, code = 3, length = 0.05, col = "blue"
)

# (3) Covariance and Pearson’s correlation coefficient: For a univariate sample, the functions
# cov() and cor() return a number, for multivariate samples, these functions returns a
# matrix. Try the following:
#   x : 10,20,30,40,50,60,70,80,90,100
# y : 95, 220, 279, 424, 499, 540, 720, 880, 950, 1200
# cov(x,y)
# cor(x,y)
# cor(longley) #multivariate data

x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
y <- c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)
cov(x, y)
cor(x, y)
cor(longley)

#########################################################################


# II. One sample tests
# (1) One sample Z test:
# (a) Write a function to perform one sample Z test, where given a data set x that is
# randomly drawn from a Gaussian distribution of population mean μ and standard
# deviation σ the function returns the conclusions of the test along with the computed
# statistic values. The function should be defined as
# one_sample_Ztest(x,sigma,muzero, alpha,null) where x is the data vector,
# sigma is the population standard deviation, muzero is the population mean for
# comparison, alpha is the significance level and null is a string indicating type of
# null hypothesis. The possible values of null should be equal, less_than_or_equal
# or more_than_or_equal. The function should return a vector with two numbers
# and the conclusion: p-value and the Z-value and the statistical conclusion.
 


one_sample_Ztest <- function(x, sigma, muzero, alpha = 0.05, null = "equal") {
  
  # Step 1: Compute sample mean and size
  xbar <- mean(x)
  n <- length(x)
  
  # Step 2: Compute Z statistic
  Z <- (xbar - muzero) / (sigma / sqrt(n))
  
  # Step 3: Compute p-value based on hypothesis type
  if (null == "equal") {
    p_val <- 2 * (1 - pnorm(abs(Z)))  # Two-tailed
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean ≠ muzero)",
                         "Fail to reject the null hypothesis (mean = muzero)")
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pnorm(Z)  # Right-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean > muzero)",
                         "Fail to reject the null hypothesis (mean ≤ muzero)")
  } else if (null == "more_than_or_equal") {
    p_val <- pnorm(Z)  # Left-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean < muzero)",
                         "Fail to reject the null hypothesis (mean ≥ muzero)")
  } else {
    stop("Invalid 'null' argument. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  # Step 4: Return result
  return(list(
    Z_value = Z,
    p_value = p_val,
    conclusion = conclusion
  ))
}



# (b) Use the data set to test the null hypothesis that μ = μ0:
#   x = c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
#         137.4, 145.6, 135.6, 135.4, 121.5)
# and μ0 of 124.6 and σ = 14.5 with 0.05 significance level.
# Define the data
x <- c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
       137.4, 145.6, 135.6, 135.4, 121.5)

# Run the Z-test using the function we created
result <- one_sample_Ztest(x, sigma = 14.5, muzero = 124.6, alpha = 0.05, null = "equal")

# Print the results
print(result)

###################################################################################################################################


# (2) One sample t- test:
# (a) Write a function to perform a one sample t-test given a data set x that is randomly
# drawn from a Gaussian distribution of population mean μ and standard deviation
# σ. The function should return the conclusions of the test along with the statistic
# values. Function should be defined as
# one_sample_t_test(x,muzero,alpha,null) where the arguments have the same
# meaning as above.
one_sample_t_test <- function(x, muzero, alpha = 0.05, null = "equal") {
  
  # Sample statistics
  xbar <- mean(x)
  s <- sd(x)
  n <- length(x)
  t_stat <- (xbar - muzero) / (s / sqrt(n))
  df <- n - 1
  
  # Determine p-value and conclusion
  if (null == "equal") {
    p_val <- 2 * (1 - pt(abs(t_stat), df))  # Two-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean ≠ muzero)",
                         "Fail to reject the null hypothesis (mean = muzero)")
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pt(t_stat, df)  # Right-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean > muzero)",
                         "Fail to reject the null hypothesis (mean ≤ muzero)")
  } else if (null == "more_than_or_equal") {
    p_val <- pt(t_stat, df)  # Left-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean < muzero)",
                         "Fail to reject the null hypothesis (mean ≥ muzero)")
  } else {
    stop("Invalid 'null' argument. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  # Return result
  return(list(
    t_value = t_stat,
    p_value = p_val,
    df = df,
    conclusion = conclusion
  ))
}

# (b) Use the data set below to test the null hypothesis that μ = μ0, where μ0 = 100 for
# 0.05 significance level.
# x = c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
#       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)

# Given data
x <- c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)

# Run the one-sample t-test
result <- one_sample_t_test(x, muzero = 100, alpha = 0.05, null = "equal")

# Print the result
print(result)

#########################################################################################################################################

# (3) One sample proportion test: In R, the functions binom.test() and prop.test() per-
#   forms the one sample proportion test. The former computes the exact binomial probabil-
#   ity, and is used when the sample sizes are small. The latter uses a normal approximation
# to the binomial distribution and can be used when n > 30. The functions are
# binom.test(x,n,p,alternative)
# prop.test(x,n,p,alternative,correct)
# where x is the number of successes, n is the total number of trials, p proportion to test
# against (i.e., hypothesized value), alternative is the string value indicating type of null
# hypothesis as “two-sided”, “less”, “greater”, and correct is a logical variable indicating
# whether a correction should be applied for small sample sizes (the default is TRUE).
# Print the results of both the tests with x = 710, n = 2600, p = 0.25 and alternative=greater.

# Given values
x <- 710
n <- 2600
p_null <- 0.25

# Exact binomial test
binom_result <- binom.test(x = x, n = n, p = p_null, alternative = "greater")

# Proportion test (normal approx.)
prop_result <- prop.test(x = x, n = n, p = p_null, alternative = "greater", correct = TRUE)

# Print results
cat("=== binom.test() Result ===\n")
print(binom_result)

cat("\n=== prop.test() Result ===\n")
print(prop_result)


#################################################################################################################################

# (4) One sample variance test:
# (a) Write a function with the following structure to output the statistical conclusion
# and properties (p-value and σ2 value):one_sample_variance_test(x,test_sigma,alpha), where the function should
# compute the χ2 test statistic and get the appropriate limits by using the qchisq()
# function and deciding the conclusion.

one_sample_variance_test <- function(x, test_sigma, alpha = 0.05) {
  n <- length(x)
  s2 <- var(x)                    # Sample variance
  sigma2_0 <- test_sigma^2       # Hypothesized variance
  
  # Chi-squared test statistic
  chi_sq <- (n - 1) * s2 / sigma2_0
  
  # Degrees of freedom
  df <- n - 1
  
  # Critical values for two-tailed test
  lower_crit <- qchisq(alpha / 2, df)
  upper_crit <- qchisq(1 - alpha / 2, df)
  
  # p-value
  p_val <- 2 * min(pchisq(chi_sq, df), 1 - pchisq(chi_sq, df))
  
  # Conclusion
  conclusion <- ifelse(chi_sq < lower_crit | chi_sq > upper_crit,
                       "Reject the null hypothesis (σ² ≠ σ₀²)",
                       "Fail to reject the null hypothesis (σ² = σ₀²)")
  
  return(list(
    chi_sq_statistic = chi_sq,
    sample_variance = s2,
    p_value = p_val,
    df = df,
    critical_values = c(lower_crit, upper_crit),
    conclusion = conclusion
  ))
}


# (b) Perform the above test for the data set given below for hypothesized σ = 29 and for
# 0.05 significance level. The points are from a normal distribution with mean=140
# and standard deviation of 20.
# x = c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
#       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)

# Provided data
x <- c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)

# Hypothesized standard deviation = 29
result <- one_sample_variance_test(x, test_sigma = 29, alpha = 0.05)

# Show the result
print(result)


########################################################################################################################################

# (5) One sample Wilcoxon signed rank test: In R, the function wilcox.test() carries out
# one- and two-sample non-parametric tests. The arguments are
# wilcox.test(x,y=NULL,alternative,mu=0,paired=FALSE,exact=NULL,correct=TRUE,conf.int=FALSE,conf.level=0.95)
# where for two samples, y will not be NULL. Perform this test for the data set with μ = 160
# and confidence level of 95% and H0 as μ >= μ0.
# x = c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
#       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)

# Given data
x <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)

# Perform the one-sample Wilcoxon signed-rank test
wilcox.test(
  x,
  mu = 160,
  alternative = "less",       # H0: median >= 160 vs H1: median < 160
  conf.int = TRUE,
  conf.level = 0.95
)


# III. Two sample tests
# (1) Two sample Z test:
# (a) Write a function to perform a two sample Z test given data sets x1, x2, their respec-tive standard deviations, siginficance level and null hypothesis. As above, the func-
# tion should return the conclusions of the test along with the statistic values. The function should be defined as two_sample_Z_test(x1,x2,sigma_x1,sigma_x2,alpha,null)

two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha = 0.05, null = "equal") {
  n1 <- length(x1)
  n2 <- length(x2)
  mean1 <- mean(x1)
  mean2 <- mean(x2)
  
  # Standard error
  se <- sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  
  # Z-statistic
  Z <- (mean1 - mean2) / se
  
  # Calculate p-value based on null type
  if (null == "equal") {
    p_val <- 2 * (1 - pnorm(abs(Z)))
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 = μ2", "Fail to reject H0")
  } else if (null == "greater_than_or_equal") {
    p_val <- pnorm(Z)
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 ≥ μ2 (Accept μ1 < μ2)", "Fail to reject H0")
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pnorm(Z)
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 ≤ μ2 (Accept μ1 > μ2)", "Fail to reject H0")
  } else {
    stop("Invalid null hypothesis type. Use 'equal', 'greater_than_or_equal', or 'less_than_or_equal'")
  }
  
  return(list(
    Z_statistic = Z,
    p_value = p_val,
    conclusion = conclusion
  ))
}


# (b) Use the data given in two-sample.dat for this problem with σx1 = 24.6 and
# σx2 = 27.8 with α = 0.05 to test the null hypothesis that the μ1 ≥ μ2.

# Read the data file (adjust path and format as necessary)
data <- read.table("two-sample.dat", header = FALSE)  # Or use read.csv()

x1 <- data$V1
x2 <- data$V2

# Given standard deviations and alpha
sigma_x1 <- 24.6
sigma_x2 <- 27.8
alpha <- 0.05

# Test H0: μ1 ≥ μ2
result <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null = "greater_than_or_equal")

# Show result
print(result)


###############################################################################################################################

# (2) Two sample t-test: In R, both one-sample and two-sample t-tests can be performed with
# the library function t.test() with the following arguments:
# t.test(x,y,alternative,mu,paired,var.equal,conf.level) where x,y are the data
# vectors, alternative is a character string specifying the alternate hypothesis of 3 pos-sible values: two.sided, less and greater. One can also input a vector with all three,
# in which case all the 3 hypotheses are tested, the default value is two.sided. For one-
# sample, mu is the hypothesized mean of the population, and for two samples, it is the
# hypothesized difference of the means. A conf.level=0.95 sets the confidence level at 95%.
# (a) Welsch’s test: use the data sets to carry out the t-test for equality of means as H0.
# Print the results summary.
# Xvar=c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
# Yvar=c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)

# Given data
Xvar <- c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
Yvar <- c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)

# Perform Welch's t-test (default is var.equal = FALSE)
t_test_result <- t.test(Xvar, Yvar, alternative = "two.sided", var.equal = FALSE)

# Print the summary
print(t_test_result)



# (b) Dependent variables: Use the data sets below to carry out a paired t-test for a
# significance level of 0.05. In this case, do we need to input μ?
# data_before = c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
# data_after = c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

# Given paired data
data_before <- c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after  <- c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

# Perform paired t-test
paired_test_result <- t.test(data_before, data_after,
                             alternative = "two.sided",
                             paired = TRUE,
                             conf.level = 0.95)

# Print summary
print(paired_test_result)

###############################################################################################################################

# (3) Two-sample proportion test: In R, the function prop.test() can perform proportion
# tests for one, two or more proportions. Here we will also learn the Fisher’s test applicable
# for small samples. The input is changed as follows:
# prop.test(x,n,p=NULL,alternative="two.sided",correct=TRUE)
# fisher.test(x,alternative="two.sided",conf.int=TRUE,conf.level=0.95)
# where x is a 2x2 matrix containing the values of the contingency table under different cat-egories for the Fisher’s test and a data vector of counts of successes for the prop.test(),
# n is a data vector containing number of trials in which x successes were observed, p is a
# vector of probabilites of successes. The alternative can be two.sided, less or more.
# (a) Perform a prop.test() test for the following problem with H0 that the proportions
# are equal. In a health survey, 520 out of 600 men and 550 out of 600 women
# questioned said they use antibiotics whenever fever continues for more than 2 days.
# We want to test whether there is a significant difference in the fraction of men and
# women who start taking antibotics after 2 days of fever.

# Number of successes
x <- c(520, 550)

# Number of trials
n <- c(600, 600)

# Two-sample proportion test (with continuity correction)
prop_test_result <- prop.test(x = x, n = n, alternative = "two.sided", correct = TRUE)

# Print results
print(prop_test_result)



# (b) Perform a Fisher’s exact test for the following data to find whether the patients from
# higher income group indulge in tobacco abuse in a significantly different proportion
# than the patients from the lower income group.
# 
# Higher-income Lower-income
# Tobacco abuse 11 17
# No abuse 42 39

# 2x2 contingency matrix
tobacco_matrix <- matrix(c(11, 42, 17, 39), nrow = 2, byrow = TRUE)

# Fisher's exact test
fisher_test_result <- fisher.test(tobacco_matrix, alternative = "two.sided", conf.int = TRUE)

# Print results
print(fisher_test_result)

#As p-value > 0.05, then there's no significant difference in tobacco use between income groups.
##############################################################################################################################

# (4) Two-sample variance test: In R, we will use the F-distribution functions (qf()) to carry
# out the two-sample variance test.
# (a) Write a function of the form two_sample_variance_test(x,y,alpha) that out-
# puts the statistical conclusion along with the statistical values of F and p-values.


two_sample_variance_test <- function(x, y, alpha = 0.05) {
  # Compute variances and sample sizes
  s1_sq <- var(x)
  s2_sq <- var(y)
  n1 <- length(x)
  n2 <- length(y)
  
  # Ensure F statistic is >1 by putting the larger variance in the numerator
  if (s1_sq > s2_sq) {
    F_stat <- s1_sq / s2_sq
    df1 <- n1 - 1
    df2 <- n2 - 1
  } else {
    F_stat <- s2_sq / s1_sq
    df1 <- n2 - 1
    df2 <- n1 - 1
  }
  
  # p-value from F-distribution (two-tailed)
  p_value <- 2 * min(
    pf(F_stat, df1, df2, lower.tail = FALSE),
    pf(F_stat, df1, df2, lower.tail = TRUE)
  )
  
  # Decision
  conclusion <- if (p_value < alpha) {
    "Reject the null hypothesis: Variances are significantly different."
  } else {
    "Fail to reject the null hypothesis: No significant difference in variances."
  }
  
  # Return results
  return(list(F_value = F_stat, p_value = p_value, conclusion = conclusion))
}



# (b) Use the data sets below to carry out this test with α = 0.05.
# x = c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,
#       1047.0,987.8,1051.0,885.2,
#       1049.5,1098.2,1001.5,1011.1,991.6)
# 
# y = c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
#       1012.3, 1040.7, 1099.5,
#       1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)

x <- c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,
       1047.0,987.8,1051.0,885.2,1049.5,1098.2,1001.5,1011.1,991.6)

y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
       1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)

# Run the test
result <- two_sample_variance_test(x, y, alpha = 0.05)
print(result)

###############################################################################################################################


# (5) Wilcoxon signed rank test for two dependent samples: This is carried out using wilcox.test()
# function in R again, and the parameters are already described above in the one sample
# tests. Carry out this test for the following data with conf.level=0.95 for the null hy-pothesis that the mean for the paired sample is greater than 0, i.e. the two samples have different means.
# Pre_therapy : 74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60
# Post_therapy : 79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54

# Data for Pre-therapy and Post-therapy
Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)

# Wilcoxon signed rank test
result <- wilcox.test(Pre_therapy, Post_therapy, alternative = "greater", conf.level = 0.95)

# Print result
print(result)

#As p-value < 0.05, we reject the null hypothesis, suggesting that the Pre-therapy values are significantly higher than the Post-therapy values.

#####################################################################################################################################


# (6) Wilcoxon rank sum test for unpaired samples and Mann-Whitney test: Use the wilcox.test()
# function to carry out the Wilcoxon rank sum test for two independent samples given be-low with the alternate hypothesis that the placebo population has a smaller mean than
# that exposed to the drug. Use a confidence level of 95%.
# drug : 31.7,75.0,101.1,60.5,62.8,59.3,58.9,91.3,99.1,52.0,39.1
# placebo : 59.3,72.7,100.5,64.7,69.0,72.7,69.6,97.4,100.6,65.1,65.7


# Data for drug and placebo
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)

# Wilcoxon rank sum test (Mann-Whitney test)
result <- wilcox.test(drug, placebo, alternative = "less", conf.level = 0.95)

# Print result
print(result)

#As p-value > 0.05, we fail to reject the null hypothesis, suggesting no significant difference between the drug and placebo populations.


###############################################################################################################################


# (7) Kruskal Wallis test: In R, this test is performed by kruska.test() function.
# Group-1 : 220 214 203 184 186 200 165
# Group-2 : 262 193 225 200 164 266 179
# Group-3 : 272 192 190 208 231 235 141
# Group-4 : 190 255 247 278 230 269 289
# Reform the data above into a (x,y) form where x stands for the value and y is the
# category of the group (use rep() function to label each data point according to the
# group), then use the above R function with arguments x and y. Print the results output
# by the function.


# Data for each group
Group_1 <- c(220, 214, 203, 184, 186, 200, 165)
Group_2 <- c(262, 193, 225, 200, 164, 266, 179)
Group_3 <- c(272, 192, 190, 208, 231, 235, 141)
Group_4 <- c(190, 255, 247, 278, 230, 269, 289)

# Combine all data into a single vector (x) for the values
x <- c(Group_1, Group_2, Group_3, Group_4)

# Create a vector (y) for the group labels using rep()
y <- rep(1:4, times = c(length(Group_1), length(Group_2), length(Group_3), length(Group_4)))

# Kruskal-Wallis test
result <- kruskal.test(x ~ y)

# Print the result
print(result)

#As p-value > 0.05, we fail to reject the null hypothesis, suggesting no significant difference between the groups.

##############################################################################################################################


# (8) Chi-square GoF test: Based on what we learnt in class, write a function to perform the
# GoF test based on input data of expected and observed values. We will use qchisq()
# function to get the critical value and pchisq() to get the p-value. Use the function to
# carry out the test for the following data:
# Observed : 32, 82, 77, 49
# Expected : 40,80,80,40

# Function to perform Chi-square Goodness of Fit test
chi_square_gof_test <- function(observed, expected, alpha = 0.05) {
  # Calculate the Chi-square statistic
  chi_square_stat <- sum((observed - expected)^2 / expected)
  
  # Degrees of freedom: (Number of categories - 1)
  df <- length(observed) - 1
  
  # Get the critical value from the Chi-square distribution
  critical_value <- qchisq(1 - alpha, df)
  
  # Calculate the p-value
  p_value <- 1 - pchisq(chi_square_stat, df)
  
  # Conclusion based on p-value
  conclusion <- ifelse(p_value < alpha, "Reject the null hypothesis", "Fail to reject the null hypothesis")
  
  # Return the results
  return(list(
    chi_square_statistic = chi_square_stat,
    degrees_of_freedom = df,
    critical_value = critical_value,
    p_value = p_value,
    conclusion = conclusion
  ))
}

# Observed and Expected values
observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)

# Perform the Chi-square Goodness of Fit test
result <- chi_square_gof_test(observed, expected)

# Print the result
print(result)

#As p_value >= 0.05, we fail to reject the null hypothesis, meaning the observed frequencies are consistent with the expected frequencies.


#################################################################################################################################################


# (1) ANOVA test on people on the Titanic ship
# (a) Read in the data file called titanic.csv. Make histogram plots of groups of
# people marked as ‘1st’, ‘2nd’ and ‘3rd’ (use about 30 bins) to check whether the
# three samples have approximately the same variance and are looking approximately
# normal. Then we are justified in using ANOVA. Make the plots in a 3x1 grid. Our
# null hypothesis is that the mean age is same among the 3 groups of people.

# (1a) Read data and create histograms
titanicData <- read.csv("/home/ibab/Downloads/titanic.csv")

# Create histograms in 3x1 grid
par(mfrow = c(3, 1))  # Set up 3x1 plotting grid

hist(titanicData$Age[titanicData$Pclass == 1], breaks = 30, 
     main = "Age Distribution - 1st Class", xlab = "Age", col = "lightblue")
hist(titanicData$Age[titanicData$Pclass == 2], breaks = 30, 
     main = "Age Distribution - 2nd Class", xlab = "Age", col = "lightgreen")
hist(titanicData$Age[titanicData$Pclass == 3], breaks = 30, 
     main = "Age Distribution - 3rd Class", xlab = "Age", col = "lightpink")

par(mfrow = c(1, 1))  # Reset plotting grid



# (b) To quantitatively verify the equal variance assumption, we are going to determine
# the mean and standard deviations from each group. Load the package dplyr, we
# will use two functions group_by() and summarise(). Study the output of the
# following commands:
# titanic_by_passenger_class<- group_by(titanicData,passenger_class)
# summarise(titanic_by_passenger_class, group_mean=mean(age,na.rm=TRUE),group_sd=sd(age,na.rm=TRUE)
# What do you find? Are the standard deviations similar between the groups? Print a statement showing the conclusion of this comparison.

# (1b) Check group means and standard deviations
library(dplyr)

titanic_by_passenger_class <- group_by(titanicData, Pclass)
group_stats <- summarise(titanic_by_passenger_class, 
                         group_mean = mean(Age, na.rm = TRUE),
                         group_sd = sd(Age, na.rm = TRUE))

print(group_stats)

# Print conclusion about variances
if (max(group_stats$group_sd)/min(group_stats$group_sd) < 2) {
  print("The standard deviations are similar enough between groups (ratio < 2) to proceed with ANOVA.")
} else {
  print("The standard deviations differ substantially between groups, violating ANOVA's equal variance assumption.")
}





# (c) We fit the ANOVA model to the data using lm() function. This function takes
# a formula and data frame as arguments. A model formula takes the form of a
# response variable followed by a tilde( ) and then at least one explanatory variable.
# Here we will give age~passenger_class which tells R to ‘fit’ a model in which
# age of passengers are grouped by the variable passenger_class. The command therefore is
# lmresults <- lm(age~passenger_class, data=titanicData)
# anova(lmresults)
# The anova() function returns the ANOVA table as output. What is your statistical
# inference/decision from the table, and therefore what is the statistical conclusion?

# (1c) Perform ANOVA
lmresults <- lm(Age ~ factor(Pclass), data = titanicData)
anova_results <- anova(lmresults)
print(anova_results)

# Print conclusion
if (anova_results$`Pr(>F)`[1] < 0.05) {
  print("We reject the null hypothesis - there is significant evidence that mean ages differ between passenger classes.")
} else {
  print("We fail to reject the null hypothesis - there is no significant evidence of difference in mean ages between passenger classes.")
}

# (d) The ANOVA tells us that at least one group has a mean different from the others,
#             but does not tell us which group means are actually different. A Tukey-Kramer’s
#             test tests the null hypothesis that there is no difference between the population
#             means of all pairs of groups. This is invoked in R by using TukeyHSD() function.
#             Execute the following command;
#             TukeyHSD(aov(lmresults))
#             Look at the columns labeled ‘diff’ and ‘p adj’. The p-values are calculated using a
#             95% confidence interval, and ‘lwr’ and ‘upr’ denote lower and upper bounds of the
#             interval. From the output you should be able to see that the CIs do not include
#             zero, and since the p-value is less than 0.05 in all the cases, the H0 is rejected for all
#             pairs, and we will conclude that the means of the three populations are significantly
#             different from each other.
#            

# (1d) Tukey-Kramer post-hoc test
tukey_results <- TukeyHSD(aov(lmresults))
print(tukey_results)





# (e) Let us also perform a Kruskal-Wallis test for the above data since the test does not
#             need us to assume normal distribution like ANOVA. Execute
#             kruskal.test(age~passenger,data=titanidData). Check whether the p value
#             leads to the same conclusion as the parametric test above.
#             

# (1e) Kruskal-Wallis test
kruskal_results <- kruskal.test(Age ~ factor(Pclass), data = titanicData)
print(kruskal_results)

# Compare with ANOVA conclusion
if (kruskal_results$p.value < 0.05) {
  print("Kruskal-Wallis test agrees with ANOVA - significant differences exist between groups.")
} else {
  print("Kruskal-Wallis test disagrees with ANOVA - no significant differences found.")
}

################################################################################################################################


# (2) Cuckoo egg size problem:
# (a) The European cuckoo does not look after its own eggs, but instead lays them in
# the nests of birds of other species. Previous studies showed that cuckoos sometimes
# have evolved to lay eggs that are colored similarly to the host bird species’ eggs.
# Is the same true of egg size – do cuckoos lay eggs similar in size to the size of the
# eggs of their hosts? The data file “cuckooeggs.csv” contains data on the lengths
# of cuckoo eggs laid in the nests of a variety of host species. Here we compare the
# mean size of cuckoo eggs found in the nests of different host species. Plot a multiple
# histogram showing cuckoo egg lengths by host species.

# Load required packages
library(ggplot2)
library(dplyr)

cuckooData <- read.csv("/home/ibab/Downloads/cuckooeggs.csv")

# Create multiple histogram
ggplot(cuckooData, aes(x = egg_length, fill = host_species)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~ host_species, ncol = 1) +
  labs(title = "Cuckoo Egg Lengths by Host Species",
       x = "Egg Length (mm)", y = "Count") +
  theme(legend.position = "none")



# (b) Calculate a table that shows the mean and standard deviation of length of cuckoo
# eggs for each host species. Look at the graph and the table. For these data, would
# ANOVA be a valid method to test for differences between host species in the lengths
# of cuckoo eggs in their nests?


host_stats <- cuckooData %>%
  group_by(host_species) %>%
  summarise(
    n = n(),
    mean_length = mean(egg_length, na.rm = TRUE),
    sd_length = sd(egg_length, na.rm = TRUE),
    se_length = sd_length/sqrt(n)
  )

print(host_stats)

# Check ANOVA assumptions
sd_ratio <- max(host_stats$sd_length)/min(host_stats$sd_length)
if (sd_ratio < 2) {
  cat("\nThe standard deviations are similar enough between groups (ratio =", 
      round(sd_ratio, 2), "< 2) to proceed with ANOVA.\n")
} else {
  cat("\nWarning: The standard deviations differ substantially between groups (ratio =",
      round(sd_ratio, 2), "≥ 2), which may violate ANOVA's equal variance assumption.\n")
}            

# (c) Use ANOVA to test for a difference between host species in the mean size of the
# cuckoo eggs in their nests. What is your conclusion?
# 

anova_model <- lm(egg_length ~ host_species, data = cuckooData)
anova_results <- anova(anova_model)
print(anova_results)

# Print conclusion
if (anova_results$`Pr(>F)`[1] < 0.05) {
  cat("\nReject the null hypothesis (p =", anova_results$`Pr(>F)`[1], 
      "): There is significant evidence that mean cuckoo egg lengths differ between host species.\n")
} else {
  cat("\nFail to reject the null hypothesis (p =", anova_results$`Pr(>F)`[1], 
      "): No significant evidence of difference in mean cuckoo egg lengths between host species.\n")
}           

# (d) Assuming that ANOVA rejected the null hypotheses of no mean differences, use a
# Tukey-Kramer test to decide which pairs of host species are significantly different
# from each other in cuckoo egg mean length. What is your conclusion?

if (anova_results$`Pr(>F)`[1] < 0.05) {
  tukey_results <- TukeyHSD(aov(anova_model))
  print(tukey_results)
  
  # Visualize the pairwise comparisons
  plot(tukey_results, las = 2)
  
  # Count significant pairs
  sig_pairs <- sum(tukey_results$host_species[, "p adj"] < 0.05)
  total_pairs <- nrow(tukey_results$host_species)
  cat("\n", sig_pairs, "out of", total_pairs, 
      "host species pairs show statistically significant differences in mean cuckoo egg length.\n")
  
  # Print significant pairs
  sig_results <- as.data.frame(tukey_results$host_species) %>%
    filter(`p adj` < 0.05) %>%
    arrange(`p adj`)
  
  if (nrow(sig_results) > 0) {
    cat("\nSignificant pairs (ordered by p-value):\n")
    print(sig_results)
  } else {
    cat("\nNo pairs show statistically significant differences after adjustment.\n")
  }
} else {
  cat("\nANOVA was not significant (p =", anova_results$`Pr(>F)`[1], 
      "), so no post-hoc tests are needed.\n")
}   


#########################################################################################################################


# (3) Maize and malaria problem:
# (a) The pollen of the maize (corn) plant is a source of food to larval mosquitoes of
# the species Anopheles arabiensis, the main vector of malaria in Ethiopia. The
# production of maize has increased substantially in certain areas of Ethiopia recently,
# and over the same time period, malaria has entered in to new areas where it was
# previously rare. This raises the question, is the increase of maize cultivation partly
# responsible for the increase in malaria?
# One line of evidence is to look for an association between maize production and
# malaria incidence at different geographically dispersed sites (Kebede et al. 2005).
# The data set “malaria vs maize.csv” contains information on several high-altitude
# sites in Ethiopia, with information about the level of cultivation of maize (low,
# medium or high in the variable maize yield) and the rate of malaria per 10,000
# people (incidence rate per ten thousand).
# Plot a multiple histogram to show the relationship between level of maize produc-tion and the incidence of malaria.


# Load required packages
library(ggplot2)
library(dplyr)
# library(multcomp)  # For post-hoc tests
malaria_data <- read.csv("/home/ibab/Downloads/malaria vs maize.csv")
# Convert maize_yield to ordered factor
malaria_data$maize_yield <- factor(malaria_data$maize_yield, levels = c("Low", "Medium", "High"),ordered = TRUE)
# Plot multiple histogram
ggplot(malaria_data, aes(x = incidence_rate_per_ten_thousand, fill = maize_yield)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  facet_wrap(~ maize_yield, ncol = 1) +
  labs(title = "Malaria Incidence Rate by Maize Yield Level",
       x = "Malaria Incidence Rate (per 10,000)", y = "Count") +
  theme_minimal() +theme(legend.position = "none")



# (b) ANOVA is a logical choice of method to test differences in the mean rate of malaria
# between sites differing in level of maize production. Calculate the standard devi-ation of the incidence rate for each level of maize yield. Do these data seem to
# conform to the assumptions of ANOVA? Describe any violations of assumptions you identify.

# Calculate summary statistics by maize yield level
yield_stats <- malaria_data %>%
  group_by(maize_yield) %>%
  summarise(n = n(),mean_rate = mean(incidence_rate_per_ten_thousand),sd_rate = sd(incidence_rate_per_ten_thousand),se_rate = sd_rate/sqrt(n))
print(yield_stats)
# Check variance assumption
sd_ratio <- max(yield_stats$sd_rate)/min(yield_stats$sd_rate)
if (sd_ratio < 2) {
  cat("\nStandard deviation ratio:", round(sd_ratio, 2), "(< 2) - Variance assumption may be acceptable.\n")
} else {
  cat("\nWarning: Standard deviation ratio:", round(sd_ratio, 2), "(≥ 2) - Variance assumption may be violated.\n")
}            




# (c) Compute the log of the incidence rate and redraw the multiple histograms for
# different levels of maize yield. Calculate the standard deviation of the log incidence
# rate for each level of maize yield. Does the log-transformed data better meet the
# assumptions of ANOVA than did the untransformed data?


# Add log-transformed incidence rate
malaria_data$log_incidence <- log10(malaria_data$incidence_rate_per_ten_thousand + 1)
# Plot log-transformed data
ggplot(malaria_data, aes(x = log_incidence, fill = maize_yield)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 10) +
  facet_wrap(~ maize_yield, ncol = 1) +
  labs(title = "Log-Transformed Malaria Incidence by Maize Yield Level",
       x = "Log10(Malaria Incidence Rate + 1)", y = "Count") +
  theme_minimal() +theme(legend.position = "none")
# Calculate stats for log-transformed data
log_stats <- malaria_data %>%
  group_by(maize_yield) %>%
  summarise(n = n(),mean_log = mean(log_incidence),
            sd_log = sd(log_incidence),
            se_log = sd_log/sqrt(n)
  )

print(log_stats)

# Check variance assumption after transformation
log_sd_ratio <- max(log_stats$sd_log)/min(log_stats$sd_log)
if (log_sd_ratio < 2) {
  cat("\nAfter log transformation - Standard deviation ratio:", round(log_sd_ratio, 2), 
      "(< 2) - Variance assumption improved.\n")
} else {
  cat("\nAfter log transformation - Standard deviation ratio:", round(log_sd_ratio, 2), 
      "(≥ 2) - Variance assumption still problematic.\n")
}            

# (d) Test for an association between maize yield and malaria incidence.

# Using original data (with potential assumption violations)
anova_original <- aov(incidence_rate_per_ten_thousand ~ maize_yield, data = malaria_data)
cat("\nANOVA results for original data:\n")
print(summary(anova_original))
# Using log-transformed data
anova_log <- aov(log_incidence ~ maize_yield, data = malaria_data)
cat("\nANOVA results for log-transformed data:\n")
print(summary(anova_log))
# Post-hoc tests if ANOVA is significant
if (summary(anova_log)[[1]]$'Pr(>F)'[1] < 0.05) {
  cat("\nPost-hoc pairwise comparisons (Tukey HSD) for log-transformed data:\n")
  tukey_results <- TukeyHSD(anova_log)
  print(tukey_results)
  
  # Visualize pairwise comparisons
  plot(tukey_results, las = 2)
  # Count significant pairs
  sig_pairs <- sum(tukey_results$maize_yield[, "p adj"] < 0.05)
  cat("\nNumber of significant pairwise comparisons:", sig_pairs, "\n")
  
  # Print significant pairs
  if (sig_pairs > 0) {
    cat("\nSignificant differences between:\n")
    print(tukey_results$maize_yield[tukey_results$maize_yield[, "p adj"] < 0.05, ])
  }
}
# Alternative non-parametric test (Kruskal-Wallis)
cat("\nKruskal-Wallis test results (non-parametric alternative):\n")
print(kruskal.test(incidence_rate_per_ten_thousand ~ maize_yield, data = malaria_data))

#####################################################################################################################################

# (4) Circadian rhythms of diseased animals:
#   (a) Animals that are infected with a pathogen often have disturbed circadian rhythms.
# (A circadian rhythm is an endogenous daily cycle in a behavior or physiological
#   trait that persists in the absence of time cues.) Shirasu-Hiza et al. (2007) wanted
# to know whether it was possible that the circadian timing mechanism itself could
# have an effect on disease. To test this idea they sampled from three groups of fruit
# flies: one “normal”, one with a mutation in the timing gene tim01, and one group
# that had the tim01 mutant in a heterozygous state. They exposed these flies to
# a dangerous bacteria, Streptococcus pneumoniae, and measured how long the flies
# lived afterwards, in days. The date file “circadian mutant health.csv” shows some
# of their data.
# Plot a histogram of each of the three groups. Do these data match the assumptions
# of an ANOVA?

# Load required packages
library(ggplot2)
library(dplyr)
library(ggpubr)  # For adding statistical comparisons to plots

# (4a) Read data and create histograms
circadian_data <- read.csv("circadian mutant health.csv")

# Clean genotype names (remove extra spaces)
circadian_data$genotype <- trimws(circadian_data$genotype)

# Create histograms for each genotype
ggplot(circadian_data, aes(x = days_to_death, fill = genotype)) +
  geom_histogram(bins = 20, color = "black") +
  facet_wrap(~ genotype, ncol = 1) +
  labs(title = "Days to Death by Genotype",
       x = "Days to Death", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Check ANOVA assumptions
# 1. Normality within each group
shapiro_tests <- circadian_data %>%
  group_by(genotype) %>%
  summarise(p_value = shapiro.test(days_to_death)$p.value)

print("Normality tests (Shapiro-Wilk):")
print(shapiro_tests)

# 2. Homogeneity of variances
levene_test <- car::leveneTest(days_to_death ~ genotype, data = circadian_data)
print("Homogeneity of variances test (Levene's test):")
print(levene_test)

# Visual assessment of assumptions
# Q-Q plots for normality check
ggqqplot(circadian_data, x = "days_to_death", facet.by = "genotype") +
  ggtitle("Q-Q Plots by Genotype")

# Boxplot for variance visualization
ggplot(circadian_data, aes(x = genotype, y = days_to_death, fill = genotype)) +
  geom_boxplot() +
  labs(title = "Days to Death by Genotype",
       x = "Genotype", y = "Days to Death") +
  theme_minimal()




#   
#   (b) Use a Kruskal-Wallis test to ask whether lifespan differs between the three groups
# of flies.            
#             

# (4b) Kruskal-Wallis test
kruskal_result <- kruskal.test(days_to_death ~ genotype, data = circadian_data)
print("Kruskal-Wallis test results:")
print(kruskal_result)

# If significant, perform pairwise comparisons
if (kruskal_result$p.value < 0.05) {
  print("Pairwise Wilcoxon rank sum tests with Holm adjustment:")
  pairwise_results <- pairwise.wilcox.test(circadian_data$days_to_death, 
                                           circadian_data$genotype,
                                           p.adjust.method = "holm")
  print(pairwise_results)
  
  # Add significance levels to plot
  comparison_plot <- ggplot(circadian_data, aes(x = genotype, y = days_to_death, fill = genotype)) +
    geom_boxplot() +
    stat_compare_means(method = "kruskal.test", label.y = max(circadian_data$days_to_death) * 1.1) +
    stat_compare_means(comparisons = list(c("tim01", "tim01 (rescued)"), 
                                          c("tim01", "wild type"),
                                          c("tim01 (rescued)", "wild type")),
                       method = "wilcox.test") +
    labs(title = "Days to Death by Genotype with Statistical Comparisons",
         x = "Genotype", y = "Days to Death") +
    theme_minimal()
  
  print(comparison_plot)
}          

