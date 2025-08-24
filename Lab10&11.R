################################ Lab 10&11 #######################################

# I. Sampling, permutations and combinations

# (1) Sampling from a vector: One can sample from a set of items with or wihout replacement. Test the result of the following commands:
#   x <- seq(1,100)
#   s <- sample(x,10)
#   Is the above result happening with or without replacement? Try the command sample(x,10,replace=TRUE)

x <- seq(1,100)
s <- sample(x,10)#sampling without replacement
print(x)
print(s)
print(sample(x,10,replace=TRUE)) #sampling with replacement


# (2) The package gtools has permutations and combinations functions that should be used as follows.
#   install.packages("gtools")
#   library(gtools)
#   x <- c("A","B","C","D")
#   per <- permutations (n=length(x), r=3,v=x,repeats.allowed=TRUE)
#   print(per)
#   comb <- combinations(n=length(x), r=3, v=x) where r is the size of the target vector, v is the source vector, n is the size of the source vector.

library(gtools)
x <- c("A", "B", "C", "D")

# Generate permutations
per <- permutations(n = length(x), r = 3, v = x, repeats.allowed = TRUE)
print(per)

# Generate combinations
comb <- combinations(n = length(x), r = 3, v = x)
print(comb)

########################################################################################################################################################


# II. Distributions


# (1) Binomial distribution: The shape of this PDF is decided by the parameter p. Use n=10, p=0.4 and m=3 to do the following:

# Set parameters
n <- 10  # Number of trials
p1 <- 0.4  # Probability of success
p2 <- 0.7  # Second probability for comparison
m <- 3  # Number of successes
# (a) Print the probability value for the above combination of numbers. The syntax is dbinom(m,n,p).
prob_value <- dbinom(m, n, p1)
cat("P(X =", m, ") =", prob_value, "\n")

# (b) Print the cumulative probability value for the above. Syntax: pbinom(m,n,p).

cum_prob <- pbinom(m, n, p1)
cat("P(X ≤", m, ") =", cum_prob, "\n")

# (c) Find the m value corresponding to cumulative probability of 0.8. Syntax: qbinom(cum_prob,n,p)

m_value <- qbinom(0.8, n, p1)
cat("m corresponding to cumulative probability of 0.8:", m_value, "\n")

# (d) Print 5 points randomly sampled from the Binomial distribution using rbinom(npts,n,p).

set.seed(123)
random_samples <- rbinom(5, n, p1)
cat("Random samples from Binomial Distribution:", random_samples, "\n")

# (e) Plot the probability density function (PDF) for the above parameters. On the same plot, plot the PDF for p=0.7 with a different colour.

x_vals <- 0:n
pdf1 <- dbinom(x_vals, n, p1)
pdf2 <- dbinom(x_vals, n, p2)
plot(x_vals, pdf1, type = "h", col = "blue", lwd = 4, xlab = "Number of Successes", 
     ylab = "Probability", main = "Binomial Distribution (PDF)", ylim = c(0, max(pdf1, pdf2)))
lines(x_vals, pdf2, type = "h", col = "red", lwd = 4)
legend("topright", legend = c("p = 0.4", "p = 0.7"), col = c("blue", "red"), lty = 1, lwd = 4)

# (f) Generate 100 and 10000 points randomly from this distribution and make a fre-quency table of the sampled points. Plot these as bar plots in a 2x1 grid.

set.seed(123)
sample_100 <- rbinom(100, n, p1)
sample_10000 <- rbinom(10000, n, p1)

# Create frequency tables
freq_100 <- table(sample_100)
print(freq_100)
freq_10000 <- table(sample_10000)
print(freq_10000)

# Plot frequency distributions in a 2x1 grid
par(mfrow = c(2,1))  # Set layout
barplot(freq_100, col = "skyblue", main = "Frequency of 100 Samples", 
        xlab = "Number of Successes", ylab = "Frequency")
barplot(freq_10000, col = "orange", main = "Frequency of 10,000 Samples", 
        xlab = "Number of Successes", ylab = "Frequency")
par(mfrow = c(1,1))  # Reset layout




# (2) Hypergeometric distribution: The functions pertaining to this distribution <distn name> hyper. The parameters are N, K, n and k (see class notes). Carry out the following:

# (a) Plot a histogram type plot of the hypergeometric probability density function with N=100, K=70, p=0.3, n=12 and add text within the plot window of the parameter
# names and their values.

# Set parameters
N <- 100  # Total population size
K <- 70   # Total number of successes in the population
n <- 12   # Sample size
p <- 0.3  # Probability (not directly used in hypergeometric distribution)
x_vals <- 0:n  # Possible values of successes in the sample

prob_values <- dhyper(x_vals, K, N-K, n)  # Compute probabilities

barplot(prob_values, names.arg = x_vals, col = "violet", main = "Hypergeometric Distribution",
        xlab = "Number of Successes in Sample", ylab = "Probability")
# Add text with parameter values
text(4, max(prob_values) * 0.9, paste("N =", N, "\nK =", K, "\nn =", n), col = "red", cex = 1.2)


# (b) Compute the cumulative probability up to x=10 and print the result after rounding off to 3 decimal places.

cum_prob <- phyper(10, K, N-K, n)
cat("Cumulative probability P(X ≤ 10):", round(cum_prob, 3), "\n")

# (c) Obtain the x value corresponding to a cumulative probability value of 0.9.

x_value <- qhyper(0.9, K, N-K, n)
cat("X value for cumulative probability of 0.9:", x_value, "\n")

# (d) Sample 5 points randomly from this distribution and print these with two significant digits.

set.seed(123)  # For reproducibility
random_samples <- rhyper(5, K, N-K, n)
cat("Random samples from Hypergeometric Distribution:", signif(random_samples, 2), "\n")




# (3) Geometric distribution: The functions pertaining to this distribution <distn name> use geom. The parameters are p and m(trial number at which the first success is observed).
# (a) Plot 2 probability density functions for this distribution in a 1x2 grid with (i)p=0.3 and (ii) p=0.8. What differences do you see?

# Set parameters
p1 <- 0.3  # Probability of success for first plot
p2 <- 0.8  # Probability of success for second plot
x_vals <- 1:10  # Number of trials (m values) at which first success is observed

# Compute probability values
pdf1 <- dgeom(x_vals - 1, p1)  # Geometric PMF for p = 0.3
pdf2 <- dgeom(x_vals - 1, p2)  # Geometric PMF for p = 0.8

# Plot the PDFs in a 1x2 grid
par(mfrow = c(1, 2))  # Set up a 1-row, 2-column plot layout
barplot(pdf1, names.arg = x_vals, col = "darkgreen", main = "Geometric PDF (p = 0.3)",
        xlab = "Trial Number (m)", ylab = "Probability")

barplot(pdf2, names.arg = x_vals, col = "orange", main = "Geometric PDF (p = 0.8)",
        xlab = "Trial Number (m)", ylab = "Probability")
par(mfrow = c(1, 1)) # Reset layout to default
#Explanation for the plots
# For p = 0.3: The probability decreases more gradually, meaning first successes occur later.
# For p = 0.8: The probability decreases much faster, meaning most first successes occur very early.


# (b) Compute the cumulative probability up to x=4.

cum_prob <- pgeom(4 - 1, 0.3)  # pgeom(x, p) gives P(X ≤ x) #reason for 4-1:If the first success happens on trial 4, there were 3 failures before it. R's pgeom() function expects the number of failures, not trials.
#So we pass 4 - 1 = 3 to pgeom()
cat("Cumulative probability P(X ≤ 4):", round(cum_prob, 3), "\n")

# (c) Compute the value of m at which the cumulative probabilty is 0.2.

m_value <- qgeom(0.2, 0.3) + 1  # qgeom(cum_prob, p) gives P(X ≤ m-1) R’s qgeom() returns the number of failures before success.The trial number (M) is one more than the failures (X), so we add +1.This adjustment ensures we are counting trials, not just failures.
cat("Trial number for cumulative probability 0.2:", m_value, "\n")

# (d) Generate 6 random deviates or sample points from this distribution with p=0.4.
set.seed(123)  # For reproducibility
random_samples <- rgeom(6, 0.4) + 1  # rgeom(n, p) generates random samples
cat("Random samples from Geometric Distribution:", random_samples, "\n")




# 4) Negative binomial distribution: The functions pertaining to this distribution uses nbinom. The parameters are p, r (number of successes desired) and y (number of failures                                                                     before r successes).
# (a) Compute and print the negative binomial probability density for y=5, r=3 and p=0.3

# Set parameters
r <- 3  # Number of successes
y <- 5  # Number of failures before r successes
p <- 0.3  # Probability of success
# Compute probability using dnbinom
nb_prob <- dnbinom(y, r, p)
# Print the result
cat("Negative Binomial Probability P(Y=5):", round(nb_prob, 4), "\n")


# (b) Compute and print the cumulative negative binomial probability density up to y=5.

cum_prob <- pnbinom(y, r, p)  # Cumulative probability P(Y ≤ 5)
cat("Cumulative Probability P(Y ≤ 5):", round(cum_prob, 4), "\n")

# (c) What is the y value corresponding to a cumulative probabilty value of 0.5? (ie the median)

y_median <- qnbinom(0.5, r, p)  # Find y for which P(Y ≤ y) = 0.5
cat("Median value of Y (cumulative probability = 0.5):", y_median, "\n")

# (d) Print 4 random points sampled from this distribution with r=3 and p=0.3.

set.seed(123)  # For reproducibility
random_samples <- rnbinom(4, r, p)  # Generate 4 random values
cat("Random samples from Negative Binomial Distribution:", random_samples, "\n")

# (e) Plot the negative binomial distribution function using r=10, p=0.3.

r <- 10  # Number of successes
x_vals <- 0:30  # Possible values for number of failures
nb_pmf <- dnbinom(x_vals, r, p)  # Compute probability mass function (PMF)

# Plot PMF
barplot(nb_pmf, names.arg = x_vals, col = "mediumvioletred", main = "Negative Binomial Distribution (r=10, p=0.3)",
        xlab = "Number of Failures (Y)", ylab = "Probability")

# (f) Generate a frequency histogram of 10,000 random deviates from this distribution with r=10 and p=0.3.

set.seed(123)  # Ensure reproducibility
samples <- rnbinom(10000, r, p)  # Generate 10,000 samples

# Plot histogram
hist(samples, breaks = 30, col = "coral", main = "Histogram of 10,000 Samples",
     xlab = "Number of Failures (Y)", probability = TRUE)



# (5) Poisson distribution: The functions pertaining to this distribution uses pois. The key parameter is λ and the discrete variable being m.
# (a) Compute and print the Poisson probability given λ = 10 and m = 7.

# Set parameters
lambda <- 10  # Mean number of occurrences
m <- 7  # Discrete variable

# Compute Poisson probability
poisson_prob <- dpois(m, lambda)

# Print the result
cat("Poisson Probability P(M=7):", round(poisson_prob, 4), "\n")

# (b) Calculate and print the cumulative probability for the same values above.

cum_prob <- ppois(m, lambda)  # Compute cumulative probability P(M ≤ 7)
cat("Cumulative Probability P(M ≤ 7):", round(cum_prob, 4), "\n")

# (c) Make two barplots showing a binomial probability distribution with n = 1000, p =0.3 and a Poisson PDF with λ = np. Do the two distributions agree? Why? Why not?

# Set parameters
n <- 1000
p <- 0.3
lambda_pois <- n * p  # Poisson equivalent of Binomial

# Generate probability mass functions
x_vals <- 0:1000  # Range of possible values
binom_pmf <- dbinom(x_vals, n, p)
poisson_pmf <- dpois(x_vals, lambda_pois)

# Plot both distributions
par(mfrow = c(1, 2))  # 1x2 grid for plots

# Binomial Distribution Plot
barplot(binom_pmf, names.arg = x_vals, col = "violetred1", main = "Binomial Distribution (n=1000, p=0.3)",
        xlab = "Number of Successes", ylab = "Probability", ylim = c(0, max(binom_pmf)))

# Poisson Distribution Plot
barplot(poisson_pmf, names.arg = x_vals, col = "maroon4", main = "Poisson Approximation (λ=300)",
        xlab = "Number of Events", ylab = "Probability", ylim = c(0, max(poisson_pmf)))

par(mfrow = c(1,1))  # Reset plot layout

# Do the two distributions agree?
# The Poisson distribution approximates a Binomial distribution well when n is large and p is small.Since 𝜆=np both distributions have the same mean.
# Why do they differ?
#   
# The Binomial distribution is bounded between [0, n] and has a slightly different shape for large n.
# 
# The Poisson distribution assumes independent events occurring in a fixed time or space and is unbounded.
# 
# For large n and small p, the Poisson distribution is a good approximation.

##overlayed graphs for the 2 distributions:
# Set parameters
n <- 1000
p <- 0.018
lambda_pois <- n * p  # Poisson equivalent of Binomial

# Generate probability mass functions
x_vals <- 0:400  # Range of possible values
binom_pmf <- dbinom(x_vals, n, p)
poisson_pmf <- dpois(x_vals, lambda_pois)

# Plot both distributions on the same graph
plot(x_vals, binom_pmf, type = "h", col = "steelblue2", lwd = 2, 
     main = "Binomial vs. Poisson Approximation", xlab = "Number of Successes/Events", 
     ylab = "Probability",ylim = c(0, max(binom_pmf, poisson_pmf)))

# Add Poisson PMF to the same plot
lines(x_vals, poisson_pmf, type = "h", col = "maroon4", lwd = 2)

# Add legend
legend("topright", legend = c("Binomial (n=1000, p=0.018)", "Poisson (λ=300)"), 
       col = c("steelblue2", "maroon4"), lwd = 2, lty = 1)















# (d) Find the quantile value corresponding to cumulative probability of 0.22 and λ = 10.

quantile_value <- qpois(0.22, lambda)  # Compute quantile
cat("Quantile value for cumulative probability 0.22:", quantile_value, "\n")

# (e) Obtain 10000 random sample points from a Poisson distribution with λ = 9 and make a histogram plot.

set.seed(123)  # For reproducibility
samples <- rpois(10000, 9)  # Generate 10,000 random values

# Plot histogram
hist(samples, breaks = 30, col = "yellow", main = "Histogram of 10,000 Poisson Samples (λ=9)",
     xlab = "Number of Events", probability = TRUE)




# (6) Gaussian distribution: The functions in R for the Gaussian/Normal distribution are
# for the unit normal distribution with the suffix norm. As we know the relevant parameters are μ and σ.
# (a) Compute and print the unit normal PDF value for μ = 12 and σ = 2.

# Parameters
mu <- 12  # Mean
sigma <- 2  # Standard deviation

# Compute normal PDF at X = 12
pdf_value <- dnorm(mu, mean = mu, sd = sigma)

# Print result
cat("Normal PDF value at μ =", mu, "and σ =", sigma, ":", round(pdf_value, 4), "\n")

# (b) Calculate and print the cumulative probability for Z = 2.0. Is this same as 1-CPDF(Z=-2)?

z_value <- 2.0
cum_prob <- pnorm(z_value)  # Compute cumulative probability P(Z ≤ 2.0)

# Compare with 1 - CPDF(Z=-2)
comparison <- 1 - pnorm(-z_value)

cat("Cumulative probability P(Z ≤ 2.0):", round(cum_prob, 4), "\n")
cat("1 - CPDF(Z = -2):", round(comparison, 4), "\n")


# (c) Plot a unit normal curve for the above parameters with X range of ±4σ and add a
# text box to the plot showing the parameter symbols and their values.

# Define X range: ±4σ around μ
x_vals <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 100)
pdf_vals <- dnorm(x_vals, mean = mu, sd = sigma)

# Plot Normal Distribution
plot(x_vals, pdf_vals, type = "l", col = "red", lwd = 2, main = "Normal Distribution (μ=12, σ=2)",
     xlab = "X", ylab = "Density")

# Add text box
text(mu, max(pdf_vals) * 0.8, paste("μ =", mu, "\nσ =", sigma), col = "blue", cex = 1.2)

# (d) Generate the 75th quantile point for a unit normal distribution with the above parameters.

quantile_75 <- qnorm(0.75, mean = mu, sd = sigma)
cat("75th Quantile Value:", round(quantile_75, 4), "\n")

# (e) Generate 10,000 random deviates from the unit normal distribution and plot a
# histogram. On top of this plot the unit normal curve from which you sampled.

set.seed(123)  # For reproducibility
samples <- rnorm(10000, mean = mu, sd = sigma)

# Plot histogram
hist(samples, breaks = 40, col = "lightseagreen", probability = TRUE, 
     main = "Histogram of 10,000 Normal Samples", xlab = "X values")

# Overlay Normal Curve
curve(dnorm(x, mean = mu, sd = sigma), col = "red", lwd = 2, add = TRUE)

# (f) Make a histogram plot of a ‘normalised’ binomial distribution with μ = np = 10
# and p = 0.5. ‘Normalised’ here means computing a variable of type W = m−np/√np(1−p)
# where m is the number of successes in n trials. On top of this, plot a unit normal
# distribution N(np,np(1-p). Do the two distributions agree?

set.seed(123)  # For reproducibility
n <- 100
p <- 0.5
mu <- n * p
sigma <- sqrt(n * p * (1 - p))
m_values <- rbinom(10000, size=n, prob=p)
W_values <- (m_values - mu) / sigma
hist(W_values, breaks=40, probability=TRUE, col="lavender",main="Normalized Binomial Distribution vs Normal Curve", xlab="W")
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkmagenta", lwd=2)


# (g) Plot 4 Poisson probability distribution functions corresponding to λ values 1, 10,100 and 1000 in the same graph. On the same graph of each Poisson PDF plot,
#  plot a unit normal distribution with Z =m−λ/ √λ. For what value of λ(s) do the two plots agree? Use a 2x2 grid for this problem.

lambda_values <- c(1, 10, 100, 1000)
par(mfrow = c(2, 2))  # 2x2 plot grid

for (lambda in lambda_values) {
  x_vals <-seq(0,3*lambda,by=1)
  poisson_pmf <- dpois(x_vals, lambda=lambda)
  
  # # Convert Poisson to standard normal form
  # z_vals <- (x_vals - lambda) / sqrt(lambda)
  # normal_pdf <- dnorm(z_vals)
  
  # Plot Poisson Distribution
  plot(x_vals, poisson_pmf, type = "h", col = "hotpink1", lwd = 2, 
       main = paste("Poisson (λ =", lambda, ")"), xlab = "X", ylab = "Probability")
  
  # Overlay Normal Approximation
  norm_vals=dnorm(x_vals,mean=lambda,sd=sqrt(lambda))
  lines(x_vals, norm_vals, col = "deepskyblue1", lwd = 2)
}
#As λ increases (especially 100 and 1000), the Poisson distribution begins to resemble the normal distribution due to the Central Limit Theorem.
par(mfrow = c(1, 1))  # Reset grid

# (h) The library MASS is used to generate two or more vectors of normally distributed random numbers that are correlated with one another to a specified degree. For example,
# xy <- mvrnorm(1000, mu=c(50,60), matrix(c(4,3.7,3.7,9),2))
# will generate two sets of 1000 numbers each with a mean of 50 for the first set and 60 for the second set. The matrix option specifies the covariance matrix of the
# variables.
library(MASS)  # Load MASS package

# Generate correlated samples
xy <- mvrnorm(1000, mu = c(50, 60), Sigma = matrix(c(4, 3.7, 3.7, 9), 2)) # uses MASS::mvrnorm() for correlated normal samples.


# (i) Execute var(xy) to check how close the variances are to our specified value – what is the covariance from these sample sets?

sample_variance <- var(xy)
cat("Sample Variance:\n", sample_variance, "\n")

# (ii) Extract the separate vectors x and y as x <- xy[,1] and y <- [xy,2] and plot them to look at the correlation. Print the individual variances as var(x)
# and var(y).

x <- xy[, 1]
y <- xy[, 2]

# Plot scatterplot
plot(x, y, main = "Scatterplot of Correlated Normal Samples", col="midnightblue",xlab = "X", ylab = "Y")

# Print individual variances
cat("Variance of X:", var(x), "\n")
cat("Variance of Y:", var(y), "\n")

# (iii) Are the two samples independent? If so then the sum of their variances should be equal to the variance of their sum.

sum_variance <- var(x + y)
cat("Variance of (X + Y):", sum_variance, "\n")
cat("Sum of individual variances:", var(x) + var(y), "\n")

# (iv) The reported covariance is var(xy). Compute the covariance using the correlation coefficient cor(x,y) and the individual variances and make sure
# this matches with the reported value.

computed_covariance <- cor(x, y) * sqrt(var(x) * var(y))
cat("Computed covariance:", computed_covariance, "\n")
cat("Reported covariance:", sample_variance[1, 2], "\n")



# (7) Uniform distribution: The function prefix here unif and the two parameters are the x-limits.
# (a) Generate 5 uniform random numbers between 0 and 1 and print them.

set.seed(123)  # Ensures reproducibility
uniform_numbers <- runif(5, min = 0, max = 1)  # Generate 5 random values
cat("Uniform random numbers between 0 and 1:", round(uniform_numbers, 4), "\n")

# (b) Generate 5 random samples from a uniform distribution between 50 and 100.

set.seed(456)
uniform_samples <- runif(5, min = 50, max = 100)
cat("Uniform samples between 50 and 100:", round(uniform_samples, 2), "\n")

# (c) Generate 10,000 uniform deviates and plot a histogram with x-limits 1 and 2.


set.seed(789)
uniform_deviates <- runif(10000, min = 1, max = 2)  # 10,000 samples

# Plot histogram
hist(uniform_deviates, breaks = 40, col = "plum", probability = TRUE,
     main = "Histogram of Uniform(1,2) Deviates", xlab = "X values", xlim = c(1, 2))

# Add theoretical uniform density line
abline(h = 1, col = "darkred", lwd = 2, lty = 2)  # Density = 1/(max-min) = 1




# (8) Exponential distribution: Since this is derived from the Poisson distribution, the primary parameter here is λ. The function suffix in R is exp.
# (a) What is the probability density corresponding to x = 3 and λ = 2?

lambda <- 2  # Rate parameter λ
x <- 3
pdf_value <- dexp(x, rate = lambda)  # Exponential PDF
cat("Exponential PDF at x =", x, "with λ =", lambda, ":", round(pdf_value, 4), "\n")

# (b) What is the quantile value corresponding to cumulative probability value of 0.995 for the above distribution?

cum_prob <- 0.995
quantile_value <- qexp(cum_prob, rate = lambda)  # Inverse CDF
cat("Quantile at cumulative probability 0.995 for λ =", lambda, ":", round(quantile_value, 4), "\n")

# (c) Plot the exponential cumulative probability distributions on the same graph for λ = 2, 10and100.

x_vals <- seq(0, 2, length.out = 100)  # X range for plotting
lambda_vals <- c(2, 10, 100)  # Different λ values

plot(x_vals, pexp(x_vals, rate = lambda_vals[1]), type = "l", col = "blue", lwd = 2,
     xlab = "x", xlim=c(0,2),ylab = "Cumulative Probability", main = "Exponential CDFs")
lines(x_vals, pexp(x_vals, rate = lambda_vals[2]), col = "red", lwd = 2)
lines(x_vals, pexp(x_vals, rate = lambda_vals[3]), col = "purple", lwd = 2)

legend("bottomright", legend = paste("λ =", lambda_vals), col = c("blue", "red", "purple"), lty = 1, lwd = 2)

# (d) Compute and print 4 random deviates from an exponential distribution with λ = 3.

set.seed(123)  # Reproducibility
random_values <- rexp(4, rate = 3)  # Generate 4 random samples
cat("Random samples from Exponential(λ=3):", round(random_values, 4), "\n")



# (9) Gamma distribution: The function suffix in R is gamma. There are two parameters α (shape option) and θ(scale option).
# (a) Plot the PDFs on the same graph with alpha values of 1,2,3,4 and θ value 4 with colors black, blue, red and magenta respectively. This is one of the two graphs on
# a 1x2 grid. Plot the second graph with θ values of 1,2,3,4 and α = 4 with colors black, blue, red and magenta respectively.

# Set up 1x2 plot grid
par(mfrow = c(1,2))  

# Define x values for plotting
x_vals <- seq(0, 20, length.out = 100)

# First plot: Varying α (shape), θ = 4
theta_fixed <- 4
alpha_vals1 <- c(1, 2, 3, 4)
colors1 <- c("black", "blue", "red", "magenta")

plot(x_vals, dgamma(x_vals, shape = alpha_vals1[1], scale = theta_fixed), type = "l", col = colors1[1], lwd = 2,
     xlab = "x", ylab = "Density", main = "Gamma PDFs (Varying α, θ = 4)", ylim = c(0, 0.15))
for (i in 2:4) {
  lines(x_vals, dgamma(x_vals, shape = alpha_vals1[i], scale = theta_fixed), col = colors1[i], lwd = 2)
}
legend("topright", legend = paste("α =", alpha_vals1), col = colors1, lty = 1, lwd = 2)

# Second plot: Varying θ (scale), α = 4
alpha_fixed <- 4
theta_vals2 <- c(1, 2, 3, 4)
colors2 <- c("black", "blue", "red", "magenta")

plot(x_vals, dgamma(x_vals, shape = alpha_fixed, scale = theta_vals2[1]), type = "l", col = colors2[1], lwd = 2,
     xlab = "x", ylab = "Density", main = "Gamma PDFs (Varying θ, α = 4)", ylim = c(0, 0.25))
for (i in 2:4) {
  lines(x_vals, dgamma(x_vals, shape = alpha_fixed, scale = theta_vals2[i]), col = colors2[i], lwd = 2)
}
legend("topright", legend = paste("θ =", theta_vals2), col = colors2, lty = 1, lwd = 2)

# (b) Compute and print the probability density corresponding to x = 6, α = 4 and θ = 1.

alpha <- 4
theta <- 1
x <- 6

pdf_value <- dgamma(x, shape = alpha, scale = theta)
cat("Gamma PDF at x =", x, "with α =", alpha, "and θ =", theta, ":", round(pdf_value, 4), "\n")

# (c) Compute and print the cumulative probablity up to x=6 for the above gamma PDF.

cum_prob <- pgamma(x, shape = alpha, scale = theta)
cat("Cumulative probability P(X ≤", x, ") for Gamma(α =", alpha, ", θ =", theta, "):", round(cum_prob, 4), "\n")

# (d) Compute the x value which corresponds to a cumulative probability of 0.95

quantile_value <- qgamma(0.95, shape = alpha, scale = theta)
cat("Quantile for cumulative probability 0.95:", round(quantile_value, 4), "\n")

# (e) Obtain 10,000 random deviates from the above gamma distribution and plot a histogram of this set.


set.seed(123)  # Ensure reproducibility
random_samples <- rgamma(10000, shape = alpha, scale = theta)

hist(random_samples, breaks = 50, probability = TRUE, col = "coral",
     main = "Histogram of 10,000 Gamma(4,1) Samples", xlab = "x")

# Overlay theoretical Gamma PDF
curve(dgamma(x, shape = alpha, scale = theta), col = "green", lwd = 2, add = TRUE)
legend("topright", legend = "Gamma PDF", col = "green", lty = 1, lwd = 2)
par(mfrow = c(1, 1))  # Reset grid


# (10) Chi-square(χ2 distribution: The suffix for the functions in R for this distribution is chisq. The key parameter is the degrees of freedom, r.
# (a) Plot the χ2 distribution with degree of freedom 2,3,5 and 10.


# Define x values
x_vals <- seq(0, 20, length.out = 200)

# Define degrees of freedom
df_vals <- c(2, 3, 5, 10)
colors <- c("blue", "red", "green", "black")

# Plot the first distribution
plot(x_vals, dchisq(x_vals, df_vals[1]), type = "l", col = colors[1], lwd = 2,
     xlab = "x", ylab = "Density", main = "Chi-Square Distributions", ylim = c(0, 0.3))

# Add the rest
for (i in 2:length(df_vals)) {
  lines(x_vals, dchisq(x_vals, df_vals[i]), col = colors[i], lwd = 2)
}

# Add legend
legend("topright", legend = paste("df =", df_vals), col = colors, lty = 1, lwd = 2)

# (b) Compute and print the probability density for x=6 and 5 degrees of freedom

df <- 5
x <- 6

pdf_value <- dchisq(x, df)
cat("Chi-Square PDF at x =", x, "with df =", df, ":", round(pdf_value, 4), "\n")

# (c) Compute and print the cumulative probability up to x=6 and 10 degrees of freedom

df <- 10
cum_prob <- pchisq(6, df)
cat("Cumulative probability P(X ≤ 6) for df =", df, ":", round(cum_prob, 4), "\n")

# (d) Obtain the 85th quantile for this distribution for 6 degrees of freedom

df <- 6
quantile_value <- qchisq(0.85, df)
cat("85th Quantile for df =", df, ":", round(quantile_value, 4), "\n")

# (e) Plot a histogram of 10,000 random deviates from this distribution with 6 degrees of freedom with 30 bins, red filled bars and text within the plot ”r=6” in an
# appropriate blank portion

set.seed(123)  # Ensures reproducibility
df <- 6
random_samples <- rchisq(10000, df)

hist(random_samples, breaks = 30, probability = TRUE, col = "hotpink",
     main = "Histogram of 10,000 χ²(6) Samples", xlab = "x", border = "black")

# Add text annotation
text(10, 0.05, "r = 6", col = "green", cex = 1.5)

# (f) Assuming μ = 2 and σ = 1 compute the variable Z2 = (x − μ)2/σ2 and plot the χ2 PDF with 1 degree of freedom.

# Define parameters
mu <- 2
sigma <- 1
x_vals <- seq(-5, 5, length.out = 200)

# Compute Z²
Z_squared <- (x_vals - mu)^2 / sigma^2

# Plot the χ² PDF with df=1
plot(x_vals, dchisq(Z_squared, df = 1), type = "l", col = "darkblue", lwd = 2,
     xlab = "x", ylab = "Density", main = "Chi-Square Distribution (df=1)")


##########################################################################################################################

# (1) CLT case with sampling from non-normal distribution: Here we will demonstrate
# the central limit theorem by taking samples from a non-normal distribution such as the
# uniform distribution. Follow the steps below.
# (i) Generate a sample set of 5 random deviates from a uniform distribution in the
# interval [0,10]. Repeat this 10,000 times. We now have 10,000 samples of 5 numbers each.

set.seed(123)  # For reproducibility
num_samples <- 10000
sample_size <- 5

samples <- matrix(runif(num_samples * sample_size, min = 0, max = 10), 
                  nrow = num_samples, ncol = sample_size,byrow=TRUE)

# (ii) Calculate the mean for each of the 10,000 samples and store it in a separate array.
# Plot this distribution of means as a histogram. Print the mean and standard deviation of the distribution of means.

sample_means <- rowMeans(samples)

# Plot histogram of sample means
hist(sample_means, breaks = 30, probability = TRUE, col = "orange", 
     main = "Central Limit Theorem Demonstration (Uniform Distribution)", 
     xlab = "Sample Mean", ylab = "Density", border = "black",xlim=c(0,10))

# Compute mean and standard deviation
mean_means <- mean(sample_means)
std_means <- sd(sample_means)
cat(sprintf("Mean of Sample Means: %.4f, Standard Deviation: %.4f\n", mean_means, std_means))

# (iii) Generate a sequence of numbers between 0 and 10 with 0.1 spacing. Obtain the
# normal probability densities using dnorm function with the calculated mean and
# standard deviation in the last question and store it as a separate vector.

x_vals <- seq(0, 10, by = 0.1)
normal_pdf <- dnorm(x_vals, mean = mean_means, sd = std_means)

# (iv) Since we have 10,000 samples, we have to scale the normal probability density
# function to our particular case (since the area is 1.0 otherwise). The height and
# bin width of a histogram are related to each other – if we doubled the bin width,
# there would be roughly twice as many numbers in the bin and the bar would be
# twice as high on the y-axis. So to get the height of the bars on our frequency scale,
# we multiply the total frequency, i.e., 10,000 by the bin width 0.5 to get 5000. This
# is the scaling factor for the PDF. Perform this and save the scaled probabilities as a separate array.

scaling_factor <- 10000*0.5  # Since 10,000 samples, bin width ~0.5
scaled_pdf <- normal_pdf * scaling_factor

# (v) Plot the normal curve on top of the histogram to see the level of agreement
# between the normal behaviour of the sample means and the normal curve.

lines(x_vals, normal_pdf, col = "blue", lwd = 2)
legend("topright", legend = c("Histogram of Sample Means", "Normal Approximation"), 
       col = c("orange", "blue"), lwd = c(1, 2), bty = "n")




# (2) CLT demo with sampling from non-normal, non-functional distribution: Here
# we demonstrate CLT by taking samples not from an intrinsic definition of the uniform distribution. Here we will create samples of dice throwing exercise.
# (i) Create 10,000 samples of single dice throw using the sample() function:   a<- sample(1:6,replace=T,10000)
# Make a plot of this distribution. You should see a uniform distribution.

set.seed(123)
a <- sample(1:6, replace = TRUE, 10000)

# Plot distribution of single dice throws
hist(a, breaks = seq(0.5, 6.5, 1), col = "plum", probability = TRUE, 
     main = "Single Dice Throw Distribution", xlab = "Dice Outcome", ylab = "Probability",
     border = "black",xlim=c(0,8),ylim=c(0,0.25))

# (ii) Throw two dice and add the scores together (this is the ancient game of craps).
# Generate a new object b similar to the above. Plot a histogram of a+b. You
# should see a triangular shape developing for the histogram.

b <- sample(1:6, replace = TRUE, 10000)
sum_two_dice <- a + b

# Plot histogram of two dice sum
hist(sum_two_dice, breaks = seq(1.5, 12.5, 1), col = "lightgreen", probability = TRUE,
     main = "Sum of Two Dice Throws", xlab = "Sum of Two Dice", ylab = "Probability",
     border = "black")

# (iii) Repeat the exercise with three dice. The histogram should start showing the
# distinct bell shape.

c <- sample(1:6, replace = TRUE, 10000)
sum_three_dice <- a + b + c

# Plot histogram of three dice sum
hist(sum_three_dice, breaks = seq(2.5, 18.5, 1), col = "orange", probability = TRUE,
     main = "Sum of Three Dice Throws", xlab = "Sum of Three Dice", ylab = "Probability",
     border = "black",xlim=c(0,20),ylim=c(0,0.20))
# (iv) Repeat this with five dice. The histogram is now very close to a normal curve.
# Use the mean and standard deviation of the 5 dice to generate a normal PDF. As
# in the last problem, one has to scale the PDF to match the height of the normal
# curve with the height of the histogram.

d <- sample(1:6, replace = TRUE, 10000)
e <- sample(1:6, replace = TRUE, 10000)
sum_five_dice <- a + b + c + d + e

# Compute mean and standard deviation
mean_five <- mean(sum_five_dice)
sd_five <- sd(sum_five_dice)

# Plot histogram of five dice sum
hist(sum_five_dice, breaks = seq(3.5, 30.5, 1), col = "yellow", probability = TRUE,
                  main = "Sum of Five Dice Throws", xlab = "Sum of Five Dice", ylab = "Density",
                  border = "black")

# Generate normal curve
x_vals <- seq(min(sum_five_dice), max(sum_five_dice), length.out = 100)
normal_pdf <- dnorm(x_vals, mean = mean_five, sd = sd_five) 

# Overlay normal curve
lines(x_vals, normal_pdf, col = "red", lwd = 3)

# Add legend
legend("topright", legend = c("Histogram of Sum", "Normal Approximation"), 
       col = c("yellow", "red"), lwd = c(2, 3), bty = "n")


###########################################################################################################################

# IV. ROC curve
# (1) Read in the white wine data into a dataframe, and create additional columns that clas-sifies the data as good or bad wine based on threshold quality scores of 6, 7, 8, 9 and 10.

# Read white wine dataset
wine_data <- read.csv("/home/ibab/Downloads/winequality-white.csv", sep = ";")

# View first few rows
print(head(wine_data))

# Function to create classification based on threshold
create_classification <- function(data, threshold) {
  return(ifelse(data$quality >= threshold, 1, 0))  # Good (1) if quality >= threshold, else Bad (0)
}

# Generate classification columns for different thresholds
wine_data$good_6 <- create_classification(wine_data, 6)
wine_data$good_7 <- create_classification(wine_data, 7)
wine_data$good_8 <- create_classification(wine_data, 8)
wine_data$good_9 <- create_classification(wine_data, 9)
wine_data$good_10 <- create_classification(wine_data, 10)

# Check the dataset after adding new classification columns
print(head(wine_data))

# (2) Use plot.roc() function as follows to plot the ROC curves for each threshold value.
# Which threshold value brings the ROC curve to the perfect classifier?
#   plot.roc(data$winequality,data$alcohol, #data vectors
#            main="title", #main plot title
#            legacy.axes=TRUE, # plots sensitivity against (1-specificity)
#            ci=TRUE, # plot confidence interval of AUC
#            print.auc=TRUE, # print values of AUC
#            identity.lwd=2, # linewidth of the 45 deg line (worst classifier)
#            print.thres=TRUE, # print best threshold on the graph


library(pROC)  # Ensure pROC is loaded
plot_roc_curve <- function(response, predictor, title) {
  # Remove NA and infinite values
  valid_idx <- complete.cases(response, predictor) & is.finite(predictor)
  response <- response[valid_idx]
  predictor <- predictor[valid_idx]
  
  # Ensure response has both 0 and 1 values
  if (length(unique(response)) < 2) {
    cat(sprintf("Skipping %s: Response does not have both classes (0 and 1)\n", title))
    return(NULL)
  }
  
  roc_curve <- roc(response, predictor)  # Compute ROC
  plot.roc(roc_curve, 
           main = title, 
           legacy.axes = TRUE, 
           ci = TRUE, 
           print.auc = TRUE, 
           identity.lwd = 2, 
           print.thres = TRUE)  # Plot ROC curve
}


# Plot ROC curves for each threshold
par(mfrow = c(2, 3))  # Arrange plots in a 2x3 grid
plot_roc_curve(wine_data$good_6, wine_data$alcohol, "ROC Curve (Threshold = 6)")
plot_roc_curve(wine_data$good_7, wine_data$alcohol, "ROC Curve (Threshold = 7)")
plot_roc_curve(wine_data$good_8, wine_data$alcohol, "ROC Curve (Threshold = 8)")
plot_roc_curve(wine_data$good_9, wine_data$alcohol, "ROC Curve (Threshold = 9)")
plot_roc_curve(wine_data$good_10, wine_data$alcohol, "ROC Curve (Threshold = 10)")
par(mfrow = c(1,1))  # Reset plot layout


compute_auc <- function(response, predictor) {
  if (length(unique(response)) < 2) {
    return(NA)  # Return NA if response has only one class
  } else {
    return(auc(roc(response, predictor)))
  }
}

# Compute AUC only when response has two levels
auc_6 <- compute_auc(wine_data$good_6, wine_data$alcohol)
auc_7 <- compute_auc(wine_data$good_7, wine_data$alcohol)
auc_8 <- compute_auc(wine_data$good_8, wine_data$alcohol)
auc_9 <- compute_auc(wine_data$good_9, wine_data$alcohol)
auc_10 <- compute_auc(wine_data$good_10, wine_data$alcohol)

# Print AUC values
cat("AUC values for different thresholds:\n")
cat(sprintf("Threshold 6: %.4f\n", auc_6))
cat(sprintf("Threshold 7: %.4f\n", auc_7))
cat(sprintf("Threshold 8: %.4f\n", auc_8))
cat(sprintf("Threshold 9: %.4f\n", auc_9))
cat(sprintf("Threshold 10: %s\n", ifelse(is.na(auc_10), "Not Computable (Only One Class)", auc_10)))




























































































































