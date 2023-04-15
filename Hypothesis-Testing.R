# Load the car package for linear hypothesis testing
library(car)
library(readxl)


# Get working directory:

getwd()


# Set the working directory to a specified path:

setwd("E:/Clark-Semester-2/Linear Regression/R-Files")

# Load EAEF dataset and select variables:

test_data <- read_excel("EAEF.xlsx")


# Generating log of variables:

test_data$LGEARN <- log(test_data$EARNINGS) 
test_data$S <- (test_data$S) 
test_data$ASVABC <- (test_data$ASVABC) 
test_data$EXP<- (test_data$EXPER) 


# OLS Regression :

Reg1<- lm((LGEARN) ~ (S) +(EXP) + (ASVABC) , data=test_data) 


# Summary Statistics:

summary(Reg1)



# Set the null hypothesis that the coefficients on S and EXP are both zero:

myH0 <- c("S", "EXP")
linearHypothesis(Reg1, myH0)

# Extract the F-statistic and p-value from the test result:

f_stat <- linearHypothesis(Reg1, myH0)$F[2]
p_val <- linearHypothesis(Reg1, myH0)$`Pr(>F)`[2]

# Set the significance level for the test:

alpha <- 0.05

# Check if the p-value is less than the significance level, and print the result:

if (p_val < alpha) {
  cat("The coefficients on S and EXP are jointly significant.\n")
} else {
  cat("The coefficients on S and EXP are not jointly significant.\n")
}

# Print the F-statistic and p-value
cat("F-statistic:", f_stat, "\n")
cat("p-value:", p_val, "\n")


# Another approach for determining significance level of variables:

# Set the null hypothesis that the coefficients on S and EXP are both zero:

myH0 <- c("S", "EXP")

# Extract the estimated coefficients and standard errors from the regression model:

coef_est <- coef(Reg1)
se_est <- summary(Reg1)$coef[, "Std. Error"]

# Calculate the t critical value for a two-sided test with n-p degrees of freedom, 
# where n is the sample size and p is the number of coefficients being tested.

n <- nrow(Reg1$model)
p <- length(myH0)
t_crit <- qt(1 - alpha/2, df = n - p)

# Calculate the t-statistic for the null hypothesis:

t_stat <- sum(coef_est[myH0])/sqrt(sum(se_est[myH0]^2))

# Calculate the p-value for the null hypothesis using a two-sided test:

p_val <- 2 * pt(-abs(t_stat), df = n - p)

# Check if the absolute value of the t-statistic is greater than the t critical value, 
# and print the result:

if (abs(t_stat) > t_crit) {
  cat("The coefficients on S and EXP are jointly significant.\n")
} else {
  cat("The coefficients on S and EXP are not jointly significant.\n")
}

# Print the t-statistic, t critical value, and p-value:

cat("t-statistic:", t_stat, "\n")
cat("t critical value:", t_crit, "\n")
cat("p-value:", p_val, "\n")



# Two-tailed t-test for equality of coefficients on S and EXP

# Set the significance level for the test
alpha <- 0.05

# Extract the coefficients and standard errors for S and EXP from the full model
b_S <- coef(Reg1)[2]
se_S <- sqrt(vcov(Reg1)[2, 2])
b_EXP <- coef(Reg1)[3]
se_EXP <- sqrt(vcov(Reg1)[3, 3])

# Calculate the t-statistic for the test of equality
t_stat <- (b_S - b_EXP) / sqrt(se_S^2 + se_EXP^2)

# Calculate the critical value of t at the specified alpha level and degrees of freedom
t_crit <- qt(1 - alpha/2, nrow(test_data) - 4)

# Check if the t-statistic is outside the critical region, and print the result
if (abs(t_stat) > t_crit) {
  cat("The coefficients on S and EXP are not equal.\n")
} else {
  cat("The coefficients on S and EXP are equal.\n")
}
