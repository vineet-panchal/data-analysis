# Name: Vineet Panchal
# Student Number: 501238284
# Course Code: CMTH380
# Date: 2024-07-31
# Project Code Submission

rm(list = ls()) # clearing the environment
setwd("/Users/vineetpanchal/Desktop/TMU/Year-1/SUMMER-2024/CMTH380/PROJECT") # setting up the working directory
print(paste("Working Directory: ", getwd())) # checking working directory

if (file.exists("data.txt")) { # checking if file exists in the working directory
  print("data.txt file found")
} else {
  stop("data.txt file not found in the working directory")
}

read_data <- read.table("data.txt", header = FALSE)
# read the data from the file

# print("First few rows of the data:")
# print(head(read_data)) : checking data read

if (ncol(read_data) < 3) { # checking if all columns of the file are read
  print("The data file does not contain at least 3 columns.")
}

sample_1 <- read_data[,1]
sample_2 <- read_data[,2]
sample_3 <- read_data[,3]
# getting data for each sample

print("")
print("PART A")
print("")

# PART A
# For each sample, plot the histogram and calculate the sample mean and sample variance.
# One of the samples is drawn from a normal distibution, determine which.

hist(sample_1, main="Histogram of Sample 1", xlab="Values", col="blue")
hist(sample_2, main="Histogram of Sample 2", xlab="Values", col="green")
hist(sample_3, main="Histogram of Sample 3", xlab="Values", col="red")
# plotting the histogram for each sample

mean_1 <- mean(sample_1)
var_1 <- var(sample_1)
mean_2 <- mean(sample_2)
var_2 <- var(sample_2)
mean_3 <- mean(sample_3)
var_3 <- var(sample_3)
# calculating the mean and variance for each sample

print(paste("Sample 1: Mean =", mean_1, "Variance =", var_1))
print(paste("Sample 2: Mean =", mean_2, "Variance =", var_2))
print(paste("Sample 3: Mean =", mean_3, "Variance =", var_3))
# printing the results

qqnorm(sample_1); qqline(sample_1, col="blue")
qqnorm(sample_2); qqline(sample_2, col="green")
qqnorm(sample_3); qqline(sample_3, col="red")
# plotting the Q-Q diagrams to check for normal distribution
# according to the diagram, sample 1 follows normal distribution

print("")
print("PART B")
print("")

# PART B
# Sample 1 is drawn from a population with unknown mean and variance. Based on sample 1,
# Caclulate the 99%, two sided confidence intervals on the mean of the population.
# Find the P-value of the test H0: sigma^2 = 0.5 vs H1: sigma^2 != 0.5

n_1 <- length(sample_1) # get the n value : 250
# paste(n_1) # check n value
mean_ci_1 <- qt(0.995, df=n_1-1) * (sqrt(var_1) / sqrt(n_1)) # calculate the confidence interval
lower_bound_1 <- mean_1 - mean_ci_1 # calclate the lower bound confidence interval
upper_bound_1 <- mean_1 + mean_ci_1 # calculate the upper bound confidence interval
print(paste("99% CI for mean of Sample 1: [", lower_bound_1, ", ", upper_bound_1, "]")) # check
# calculating the 99% two-sided confidence interval for the mean of sample 1

test_stat_1 <- ((n_1 - 1) * var_1) / (0.5) # calculate test statistic
paste(test_stat_1) # check test statistic 
p_value_1 <- 2 * (1 - pchisq(test_stat_1, df=n_1-1)) # calculate the p-value for sample 1
# Hypothesis test H0: sigma^2 = 0.5 vs H1: sigma^2 != 0.5

print(paste("P-value for the hypothesis test: ", p_value_1)) # check p-value for sample 1

print("")
print("PART C")
print("")

# PART C
# Sample 3 is drawn from the production of a manufacturing company. The value 1 in the data
# represents a non defective part and the value 0 represents a defective part. 
# The company wants to demonstrate that the defective parts are less that 10% of their 
# production. Conduct a hypothesis test to demonstrate this claim. 

p_hat_3 <- mean_3 # calculate sample proportion : 0.96
n_3 <- length(sample_3) # calculate n for sample 3 : 250
p0_3 <- 0.10 # hypothesized proportion
se_3 <- sqrt(((p0_3) * (1 - p0_3)) / n_3) # calculate standard error
z_stat_3 <- (p_hat_3 - p0_3) / se_3 # calculate z statistic
paste(z_stat_3) # check z statistic
p_value_3 <- pnorm(z_stat_3) # calculate p-value for sample 3
# Hypothesis test for defective parts being less than 10%

print(paste("P-value for defective parts hypothesis test: ", p_value_3)) # check p-value for sample 3