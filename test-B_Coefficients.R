library(testthat)

test_that("Beta estimation runs correctly", {
  # Set up sample data
  observed_data <- c(1, 2, 5, 7, 8)
  S <- c(1000, 950, 900, 850, 800)
  I <- c(1, 2, 3, 4, 5)

  # Estimate beta
  beta_est <- estimate_beta(observed_data, S, I)

  # Check if beta is a numeric value
  expect_true(is.numeric(beta_est))

  # Check if the estimated beta is within a reasonable range
  expect_true(beta_est > 0 && beta_est < 1)

  # Additional test: Check that beta is not negative (it's a rate)
  expect_true(beta_est >= 0)
})

test_that("Beta estimation with zero observed infections returns NA", {
  # Set up sample data where observed_data is zero
  observed_data <- c(0, 0, 0, 0, 0)
  S <- c(1000, 950, 900, 850, 800)
  I <- c(1, 2, 3, 4, 5)

  # Estimate beta
  beta_est <- estimate_beta(observed_data, S, I)

  # Check if the estimated beta is NA or zero due to no infections
  expect_true(is.na(beta_est) || beta_est == 0)
})

test_that("Beta estimation works for small and large datasets", {
  # Set up sample data for a larger dataset
  observed_data <- rep(10, 100)
  S <- rep(1000, 100)
  I <- rep(5, 100)

  # Estimate beta
  beta_est <- estimate_beta(observed_data, S, I)

  # Check if the result is within a reasonable range (since data is uniform, beta should be reasonable)
  expect_true(beta_est > 0 && beta_est < 1)
})

