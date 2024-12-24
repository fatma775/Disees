library(testthat)

test_that("SIR model runs correctly", {
  # Define parameters for the test
  beta <- 0.3      # Infection rate
  gamma <- 0.1     # Recovery rate
  S0 <- 1000       # Initial susceptible population
  I0 <- 10         # Initial infected population (non-zero)
  R0 <- 0          # Initial recovered population
  time_steps <- 50 # Number of time steps for the simulation

  # Run the SIR model
  result <- SIR_model(beta = beta, gamma = gamma, S0 = S0, I0 = I0, R0 = R0, time_steps = time_steps)

  # Check that susceptible population (S) decreases or stays constant
  expect_true(all(diff(result$S) <= 0), "S population should decrease or stay constant")

  # Check that infected population (I) increases initially
  expect_true(result$I[2] > result$I[1], "I population should increase initially")

  # Check that recovered population (R) increases over time
  expect_true(all(diff(result$R) >= 0), "R population should increase over time")

  # Ensure that populations do not go negative
  expect_true(all(result$S >= 0), "S population should not have negative values")
  expect_true(all(result$I >= 0), "I population should not have negative values")
  expect_true(all(result$R >= 0), "R population should not have negative values")
})
