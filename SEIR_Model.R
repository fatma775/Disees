#' Susceptible-Exposed-Infected-Recovered (SEIR) Model
#'
#' This function simulates the dynamics of an epidemic using the SEIR model, which adds an Exposed compartment.
#'
#' @param beta Infection rate.
#' @param sigma Rate at which exposed individuals become infectious.
#' @param gamma Recovery rate.
#' @param S0 Initial number of susceptible individuals.
#' @param E0 Initial number of exposed individuals.
#' @param I0 Initial number of infected individuals.
#' @param R0 Initial number of recovered individuals.
#' @param time_steps Number of time steps for the simulation.
#'
#' @return A list containing four elements: S (susceptible), E (exposed), I (infected), and R (recovered) at each time step.
#'
#' @examples
#' SEIR_model(beta = 0.3, sigma = 0.2, gamma = 0.1, S0 = 1000, E0 = 0, I0 = 1, R0 = 0, time_steps = 50)
#'
#' @export
SEIR_model <- function(beta, sigma, gamma, S0, E0, I0, R0, time_steps) {
  S <- numeric(time_steps)
  E <- numeric(time_steps)
  I <- numeric(time_steps)
  R <- numeric(time_steps)

  S[1] <- S0
  E[1] <- E0
  I[1] <- I0
  R[1] <- R0

  for (t in 2:time_steps) {
    dS <- -beta * S[t-1] * I[t-1]
    dE <- beta * S[t-1] * I[t-1] - sigma * E[t-1]
    dI <- sigma * E[t-1] - gamma * I[t-1]
    dR <- gamma * I[t-1]

    S[t] <- S[t-1] + dS
    E[t] <- E[t-1] + dE
    I[t] <- I[t-1] + dI
    R[t] <- R[t-1] + dR
  }

  return(list(S = S, E = E, I = I, R = R))
}
