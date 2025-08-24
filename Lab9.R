######################################## Lab9 ##################################


# Exercise1: Plot the point (2,4) with square point character type and magenta color.
plot(2,4,pch=0,col="magenta")


# Exercise2: Create a seqence of function values corresponding to sin(x) and cos(x) function from −π to π and plot the two functions on the same graph with appropriate titles, axes labels,
#blue color for the sine curve, red color for the cosine curve, star point character type for sine, cross type point character for cosine overlaid by lines joining the points.
x <- seq(-pi, pi, 0.1)  # Generate x values from -π to π
y1 <- sin(x)            # Compute sine values
y2 <- cos(x)            # Compute cosine values

# Plot sine function with blue star points
plot(x, y1,type='o', col="blue", pch=8, xlab="x", ylab="y = sin(x) / y = cos(x)", 
     main="Sine vs Cos Function")

# Add cosine function with red cross points and connecting lines
#points(x, y2, col="red", pch=4)
lines(x, y2, col="red",type='o',pch=4)

# Add legend to the plot
legend("topright", inset=0.05, lty=1, legend=c("Sine", "Cos"), 
       col=c("blue", "red"), pch=c(8, 4), title="Sine/Cos Function")


#Exercise3: Reproduce the bar graph type of plot in Fig. 4.2.1 in the Biostatistics book by Daniel using the appropriate settings for the appearance.

# Define the number of programs and their frequencies
programs <- c(1, 2, 3, 4, 5, 6, 7, 8)
frequency <- c(62, 47, 39, 39, 58, 37, 4, 11)

# Convert to probability
total_families <- sum(frequency)
probability <- frequency / total_families

# Create the bar plot
bp <- barplot(probability, names.arg = programs, col = "gray90", border = "black",
              xlab = expression(italic(x) ~ "(number of assistance programs)"), 
              ylab = "Probability", 
              main = "", # No title in image
              ylim = c(0, 0.25), space = 0.5, axes=FALSE)

# Add X-axis with inward ticks
axis(1, at = bp, labels = programs, line = 0, col = "black", tck = 0.02)

# Add Y-axis with inward ticks
axis(2, at=seq(0,0.25,0.05), tck = 0.02, las=1)

# Explicitly add the X-axis line at y=0
abline(h=-0.0002, col="black", lwd=3, lty=1)


## Add grid lines
#grid(nx = NA, ny = NULL, lty = "dotted", col = "darkgray")

## Add frequency values above bars
#text(x = bp, y = frequency+2, labels = frequency, cex = 0.8)


#Exercise4: 

# Set up a 2x3 plotting grid
par(mfrow=c(2,3))

# (i) x vs cos(x) with red color and lines
x <- seq(-pi, pi, length.out=100)
plot(x, cos(x), type='l', col='red', lwd=2, main="Cos(x)", xlab="x", ylab="cos(x)")

# (ii) x vs (x^2 / 3) + 4.2 with violet color, points and lines, linewidth 2 and linetype 1
x <- seq(-5, 5, length.out=100)
y <- (x^2 / 3) + 4.2
plot(x, y, type='o', col='purple', lwd=2, lty=1, pch=16, main="Quadratic", xlab="x", ylab="(x^2 / 3) + 4.2")

# (iii) and (iv)

# Custom function to compute binomial probability using factorial
binomial_prob <- function(k, n, p) {
  binomial_coeff <- factorial(n) / (factorial(k) * factorial(n - k))
  return(binomial_coeff * (p^k) * ((1 - p)^(n - k)))
}

# Set parameters for first plot (p=0.3)
n <- 12
p <- 0.3
x_vals <- 0:n

# Compute probabilities
probs <- sapply(x_vals, function(k) binomial_prob(k, n, p))

# Plot histogram for Binomial distribution (p=0.3)
barplot(probs, names.arg = x_vals, col = "gray", border = "black",
        main = paste("Binomial Dist (n=", n, ", p=", p, ")"), 
        xlab = "Number of Successes", ylab = "Probability")

# Set parameters for second plot (p=0.8)
p <- 0.8
probs <- sapply(x_vals, function(k) binomial_prob(k, n, p))

# Plot histogram for Binomial distribution (p=0.8)
barplot(probs, names.arg = x_vals, col = "gray", border = "black",
        main = paste("Binomial Dist (n=", n, ", p=", p, ")"), 
        xlab = "Number of Successes", ylab = "Probability")


# (v) Histogram plot using type='h' option
x <- seq(1, 10, 0.5)
y <- 50*x / (x + 2)
colors <- rep(c("blue", "orange"), length.out=length(x))
plot(x, y, type='h', col=colors, lwd=2, main="Histogram", xlab="x", ylab="50x / (x+2)")

# (vi) x vs log(x) with orange color and ‘step’ linetype
x <- seq(1, 10, length.out=100)  # Avoid log(0)
plot(x, log(x), type='s', col='orange', lwd=2, main="Log(x) Step", xlab="x", ylab="log(x)")

par(mfrow = c(1, 1))


#Exercise5: Write a script to recreate the plot in the slide with the plot title ’This is a graph’.

# Define the data
Time <- 1:10
Performance <- c(2, 6, 7, 5, 6, 9, 10, 8, 9, 10)

# Create an empty plot
plot(Time, Performance, type = "n", main = "This is a graph", 
     xlab = "Time", ylab = "Performance", 
     col.main = "blue", font.main = 2, cex.main = 1.5, 
     col.lab = "black", cex.lab = 1.2, 
     xaxt = "n", yaxt = "n")

# Add custom axes
axis(1, at = seq(2, 10, by = 2))  # X-axis ticks
axis(2, at = seq(2, 10, by = 2))  # Y-axis ticks

# Add the dashed line
lines(Time, Performance, lty = 2, col = "pink", lwd = 2)

# Add the points
points(Time, Performance, col = "pink", pch = 16, cex = 2)

# Add text labels at points
text(Time, Performance, labels = Performance, col = "red", pos = 3, cex = 1.2)

# Add a legend
legend("topleft", legend = "Per curve", col = "pink", lty = 2, pch = 16, box.lty = 1)


#Ex6: Plot a histogram representation of hypergeometric distribution with N=500, K=50 and n=30

# Function to compute factorial
factorial_custom <- function(n) {
  if (n == 0) return(1)
  prod(1:n)
}

# Function to compute combinations (nCr)
combinations <- function(n, r) {
  if (r > n) return(0)
  exp(lfactorial(n) - (lfactorial(r) + lfactorial(n - r))) # Uses logarithm of factorials
}


# Function to compute hypergeometric distribution
hypergeom_distribution_manual <- function(N, K, n) {
  x <- 0:n
  probabilities <- numeric(length(x)) # Initialize vector to store probabilities
  
  for (i in 1:length(x)) {
    num_successes <- x[i]
    numerator <- combinations(K, num_successes) * combinations(N - K, n - num_successes)
    denominator <- combinations(N, n)
    probabilities[i] <- numerator / denominator
  }
  
  return(list(x = x, probabilities = probabilities))
}

# Parameters
N <- 500  # Population size
K <- 50   # Number of successes in the population
n <- 30   # Number of draws

# Compute the hypergeometric distribution
hypergeom_data <- hypergeom_distribution_manual(N, K, n)

# Plot the histogram
barplot(hypergeom_data$probabilities, 
        names.arg = hypergeom_data$x,
        col = "skyblue", 
        border = "black",
        main = "Histogram of Hypergeometric Distribution",
        xlab = "Number of Successes",
        ylab = "Probability",
        ylim = c(0, max(hypergeom_data$probabilities) * 1.2))
grid()



#Exercise7: Write few lines of script to explore what happens to a hypergeomtric distribution when n is increased and gets closer to N. Show that it approaches the binomial distribution by
#plotting histograms of both hypergeometric and binomial on the same plot. Use a 3x3 grid of 9 graphs to show the convergence of hypergeometric to binomial distribution.

# Function to compute factorial
factorial_custom <- function(n) {
  if (n == 0) return(1)
  prod(1:n)
}

# Function to compute combinations (nCr)
combinations <- function(n, r) {
  if (r > n) return(0)
  factorial_custom(n) / (factorial_custom(r) * factorial_custom(n - r))
}

# Function to compute hypergeometric probabilities
hypergeom_distribution <- function(N, K, n, x) {
  numerator <- combinations(K, x) * combinations(N - K, n - x)
  denominator <- combinations(N, n)
  return(numerator / denominator)
}


# Function to compute binomial probabilities
binom_distribution <- function(n, p, x) {
  combinations(n, x) * (p^x) * ((1 - p)^(n - x))
}

# Plot function without libraries
plot_histogram <- function(hyper_probs, binom_probs, x_vals, n) {
  plot(NULL, xlim = c(min(x_vals), max(x_vals)), ylim = c(0, max(c(hyper_probs, binom_probs))),
       xlab = "Number of Successes", ylab = "Probability",
       main = paste("n =", n), type = "n")
  
  # Plot hypergeometric probabilities
  rect(x_vals - 0.25, 0, x_vals, hyper_probs, col = "skyblue", border = "black")
  
  # Plot binomial probabilities
  rect(x_vals, 0, x_vals + 0.25, binom_probs, col = "orange", border = "black")
  
  legend("topright", legend = c("Hypergeometric", "Binomial"), fill = c("skyblue", "orange"))
}

# Parameters
N <- 100  # Population size
K <- 40   # Number of successes in the population
n_values <- seq(10, N, length.out = 9) # Sample sizes to explore
p <- K / N # Probability of success in binomial distribution

# Create a 3x3 grid of plots
par(mfrow = c(3, 3)) # Setup 3x3 plotting grid
for (n in n_values) {
  x_vals <- 0:n
  
  # Compute hypergeometric probabilities
  hyper_probs <- sapply(x_vals, function(x) hypergeom_distribution(N, K, n, x))
  
  # Compute binomial probabilities
  binom_probs <- sapply(x_vals, function(x) binom_distribution(n, p, x))
  
  # Plot the histograms
  plot_histogram(hyper_probs, binom_probs, x_vals, n)
}

# Reset the plotting grid
par(mfrow = c(1, 1))


#Exercise8: On the same plot, draw 3 Poisson distributions with λ values of 3,20,45 (Code the probability distribution function).

# Function to compute factorial
factorial_custom <- function(n) {
  if (n == 0) return(1)
  prod(1:n)
}

# Function to compute Poisson probability distribution function
poisson_pmf <- function(lambda, x) {
  (lambda^x * exp(-lambda)) / factorial_custom(x)
}

# Define the range of x values
x_vals <- 0:60  # Choose a range wide enough to capture all distributions

# Define lambda values
lambdas <- c(3, 20, 45)

# Compute probabilities for each lambda
poisson_probs <- lapply(lambdas, function(lambda) sapply(x_vals, function(x) poisson_pmf(lambda, x)))

# Set up the plot
plot(x_vals, poisson_probs[[1]], type = "h", lwd = 2, col = "blue",
     main = "Poisson Distributions (λ = 3, 20, 45)",
     xlab = "x (Number of Events)", ylab = "Probability",
     ylim = c(0, max(unlist(poisson_probs))))
lines(x_vals, poisson_probs[[2]], type = "h", lwd = 2, col = "green")
lines(x_vals, poisson_probs[[3]], type = "h", lwd = 2, col = "red")

# Add a legend
legend("topright", legend = c("λ = 3", "λ = 20", "λ = 45"),
       fill = c("blue", "green", "red"))


#Exercise9: Load the csv file for heights and weights of 25000 people and do the following:

#(i) Plot a histogram of the height variable and determine it’s mean and standard deviation
data=read.csv("C:/Users/Aritri Baidya/Downloads/SOCR-HeightWeight.csv")
print(dim(data))
print(colnames(data))
print(rownames(data))
print(head(data,30))
height <- data$Height.Inches.  

mean_height <- mean(height, na.rm = TRUE)
sd_height <- sd(height, na.rm = TRUE)

hist(height, 
     breaks = 30,  # Set an initial bin size
     col = "skyblue", 
     border = "black",
     main = "Histogram of Heights",
     xlab = "Height",
     ylab = "Frequency")
abline(v = mean_height, col = "red", lwd = 2, lty = 2)  # Add a vertical line for the mean
print(mean_height)
print(sd_height)


#(ii) Plot a histogram of the weight variable and determine it’s mean and standard deviation
weight <- data$Weight.Pounds.  

mean_weight <- mean(weight, na.rm = TRUE)
sd_weight <- sd(weight, na.rm = TRUE)

hist(weight, 
     breaks = 30,  # Set an initial bin size
     col = "lightgreen", 
     border = "black",
     main = "Histogram of Weights",
     xlab = "Weight",
     ylab = "Frequency")
abline(v = mean_weight, col = "red", lwd = 2, lty = 2)  # Add a vertical line for the mean
print(mean_weight)
print(sd_weight)

#(iii) Draw a Gaussian curve (recall the Gaussian PDF) with the above calculated mean and standard deviation for both height and weight variables as Z vs P(Z) (i.e. Ztransformed).
#Plot using either plot() function or curve() function.
gaussian_curve <- function(z, mu, sigma) {
  (1 / (sigma * sqrt(2 * pi))) * exp(-((z - mu)^2) / (2 * sigma^2))
}

# Generate Z values for height
z_height <- seq(mean_height - 4 * sd_height, mean_height + 4 * sd_height, length = 100)
p_height <- gaussian_curve(z_height, mean_height, sd_height)

# Plot Gaussian curve for Height
plot(z_height, p_height, 
     type = "l", col = "blue", lwd = 2,
     main = "Gaussian Curve for Heights",
     xlab = "Z-Transformed Height", ylab = "P(Z)")

# Generate Z values for weight
z_weight <- seq(mean_weight - 4 * sd_weight, mean_weight + 4 * sd_weight, length = 100)
p_weight <- gaussian_curve(z_weight, mean_weight, sd_weight)

# Plot Gaussian curve for Weight
lines(z_weight, p_weight, col = "red", lwd = 2)  # Add weight Gaussian to the same plot
legend("topright", legend = c("Height", "Weight"), col = c("blue", "red"), lty = 1, lwd = 2)

#(iv) What happens when you decrease the size of the bins in the histogram plots? Make a 1x3 grid of 3 plots that show the trend for decreasing bin sizes.
par(mfrow = c(1, 3))  # Set up a 1x3 grid for plots

# Plot histogram with fewer bins (wide bins)
hist(height, breaks = 10, col = "skyblue", main = "Bins = 10", xlab = "Height", ylab = "Frequency")

# Plot histogram with moderate bins
hist(height, breaks = 30, col = "skyblue", main = "Bins = 30", xlab = "Height", ylab = "Frequency")

# Plot histogram with more bins (narrow bins)
hist(height, breaks = 100, col = "skyblue", main = "Bins = 100", xlab = "Height", ylab = "Frequency")

# Reset plot layout
par(mfrow = c(1, 1))


#Exercise10: Plot the PDF and CPDF for the uniform distribution U(1,2). Find a way to shade the region under the PDF up to x = 1.5.

# Define the range
x <- seq(0, 3, length.out = 100)
# PDF of U(1,2)
pdf_uniform <- ifelse(x >= 1 & x <= 2, 1, 0)
# CDF of U(1,2)
cdf_uniform <- pmax(0, pmin(x - 1, 1))
# Plot PDF
plot(x, pdf_uniform, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "x", main = "PDF and CDF of U(1,2)")
polygon(c(1, seq(1, 1.5, length.out = 50), 1.5), c(0, rep(1, 50), 0), col = rgb(1, 0, 0, 0.5), border = NA) # Shading
lines(x, pdf_uniform, col = "blue", lwd = 2)
# Plot CDF
plot(x, cdf_uniform, type = "l", col = "red", lwd = 2, ylab = "Cumulative Probability", xlab = "x", main = "CDF of U(1,2)")




#Exercise11:Plot the PDF and CPDF for the exponential distribution with λ = 10. Shade the region under the PDF up to x = 2.8.

# Define the range
x <- seq(0, 5, length.out = 100)
# PDF and CDF of Exponential(10)
pdf_exp <- dexp(x, rate = 10)
cdf_exp <- pexp(x, rate = 10)
# Plot PDF
plot(x, pdf_exp, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "x", main = "PDF and CDF of Exp(10)")
polygon(c(0, seq(0, 2.8, length.out = 50), 2.8), c(0, dexp(seq(0, 2.8, length.out = 50), rate = 10), 0), col = rgb(1, 0, 0, 0.5), border = NA)
lines(x, pdf_exp, col = "blue", lwd = 2)
# Plot CDF
plot(x, cdf_exp, type = "l", col = "red", lwd = 2, ylab = "Cumulative Probability", xlab = "x", main = "CDF of Exp(10)")




#Exercise 12: Plot the PDF and CPDF for the Gamma distribution with α = 5 and θ = 3.

# Define the range
x <- seq(0, 30, length.out = 100)
# PDF and CDF of Gamma(5,3)
pdf_gamma <- dgamma(x, shape = 5, scale = 3)
cdf_gamma <- pgamma(x, shape = 5, scale = 3)
# Plot PDF
plot(x, pdf_gamma, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "x", main = "PDF and CDF of Gamma(5,3)")
# Plot CDF
plot(x, cdf_gamma, type = "l", col = "red", lwd = 2, ylab = "Cumulative Probability", xlab = "x", main = "CDF of Gamma(5,3)")



#Exercise13: Plot the PDF and CPDF for the Chi-square distribution for 20 degrees of freedom. Shade the region under the PDF up to x = 1.0.

# Define the range
x <- seq(0, 50, length.out = 100)
# PDF and CDF of Chi-square(20)
pdf_chisq <- dchisq(x, df = 20)
cdf_chisq <- pchisq(x, df = 20)
# Plot PDF
plot(x, pdf_chisq, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "x", main = "PDF and CDF of Chi-square(20)")
polygon(c(0, seq(0, 1, length.out = 50), 1), c(0, dchisq(seq(0, 1, length.out = 50), df = 20), 0), col = rgb(1, 0, 0, 0.5), border = NA)
lines(x, pdf_chisq, col = "blue", lwd = 2)
# Plot CDF
plot(x, cdf_chisq, type = "l", col = "red", lwd = 2, ylab = "Cumulative Probability", xlab = "x", main = "CDF of Chi-square(20)")

