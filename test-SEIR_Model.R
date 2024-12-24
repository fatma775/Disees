library(testthat)

test_that("SEIR model runs correctly", {
  beta <- 0.3      # Infection rate
  sigma <- 0.2     # Rate at which exposed individuals become infectious
  gamma <- 0.1     # Recovery rate
  S0 <- 1000       # Initial susceptible population
  E0 <- 10         # Initial exposed individuals (not 0)
  I0 <- 1          # Initial infected individuals (non-zero)
  R0 <- 0          # Initial recovered individuals
  time_steps <- 50 # Number of time steps for the simulation

  result <- SEIR_model(beta, sigma, gamma, S0, E0, I0, R0, time_steps)

  # Check that infected population (I) increases initially
  expect_true(result$I[2] > result$I[1], "I population should increase initially")
})
