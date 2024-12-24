#' Estimate Infection Rate (Beta) from Data
#'
#' This function estimates the infection rate (beta) using maximum likelihood estimation (MLE) based on observed infection data.
#'
#' @param observed_data A numeric vector of observed new infections at each time step.
#' @param S A numeric vector of susceptible individuals at each time step.
#' @param I A numeric vector of infected individuals at each time step.
#'
#' @return The estimated infection rate (beta).
#'
#' @examples
#' observed_data <- c(1, 2, 5, 7, 8)
#' S <- c(1000, 950, 900, 850, 800)
#' I <- c(1, 2, 3, 4, 5)
#' estimate_beta(observed_data, S, I)
#'
#' @export
estimate_beta <- function(observed_data, S, I) {
  # Handle the case where there are zero observed infections
  if (all(observed_data == 0)) {
    return(NA)  # Or return 0, depending on your expected behavior
  }

  likelihood <- function(beta) {
    lambda <- beta * S * I
    -sum(dpois(observed_data, lambda, log = TRUE))
  }

  result <- optimize(likelihood, interval = c(0, 1), maximum = FALSE)
  return(result$minimum)
}
