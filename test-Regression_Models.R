library(testthat)



# Sample data frame for testing
data <- data.frame(
  Y = c(1, 0, 1, 1, 0),  # Dependent variable (binary for logistic regression)
  X1 = c(1, 2, 3, 4, 5),  # Independent variable 1
  X2 = c(5, 4, 3, 2, 1)   # Independent variable 2
)

# Linear Regression Example
linear_result <- linear_regression(Y ~ X1 + X2, data = data)
print(linear_result)

# Logistic Regression Example
logistic_result <- logistic_regression(Y ~ X1 + X2, data = data)
print(logistic_result)

# Poisson Regression Example
poisson_result <- poisson_regression(Y ~ X1 + X2, data = data)
print(poisson_result)
