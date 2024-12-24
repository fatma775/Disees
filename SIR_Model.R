#' Susceptible-Infected-Recovered (SIR) Model
#'
#' This function simulates the dynamics of an epidemic using the SIR model.
#'
#' @param beta Infection rate.
#' @param gamma Recovery rate.
#' @param S0 Initial number of susceptible individuals.
#' @param I0 Initial number of infected individuals.
#' @param R0 Initial number of recovered individuals.
#' @param time_steps Number of time steps for the simulation.
#'
#' @return A list containing three elements: S (susceptible), I (infected), and R (recovered) at each time step.
#'
#' @examples
#' SIR_model(beta = 0.3, gamma = 0.1, S0 = 1000, I0 = 1, R0 = 0, time_steps = 50)
#'
#' @export
SIR_model <- function(beta, gamma, S0, I0, R0, time_steps) {
  S <- numeric(time_steps)
  I <- numeric(time_steps)
  R <- numeric(time_steps)

  # Initial conditions
  S[1] <- S0
  I[1] <- I0
  R[1] <- R0

  for (t in 2:time_steps) {
    dS <- -beta * S[t-1] * I[t-1] / (S[t-1] + I[t-1] + R[t-1])  # Corrected the formula
    dI <- beta * S[t-1] * I[t-1] / (S[t-1] + I[t-1] + R[t-1]) - gamma * I[t-1]
    dR <- gamma * I[t-1]

    # Update populations
    S[t] <- max(0, S[t-1] + dS)  # Ensure S is never negative
    I[t] <- max(0, I[t-1] + dI)  # Ensure I is never negative
    R[t] <- max(0, R[t-1] + dR)  # Ensure R is never negative
  }

  return(list(S = S, I = I, R = R))
}
