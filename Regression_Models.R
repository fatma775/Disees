#' Linear Regression Model
#'
#' This function fits a linear regression model to predict continuous outcomes based on predictor variables.
#'
#' @param formula A formula object (e.g., Y ~ X1 + X2).
#' @param data A data frame containing the variables.
#'
#' @return A summary of the fitted linear regression model.
#'
#' @examples
#' linear_regression(Y ~ X1 + X2, data = data)
#'
#' @export
linear_regression <- function(formula, data) {
  model <- lm(formula, data = data)
  return(summary(model))
}

#' Logistic Regression Model
#'
#' This function fits a logistic regression model for binary outcomes.
#'
#' @param formula A formula object (e.g., Y ~ X1 + X2).
#' @param data A data frame containing the variables.
#'
#' @return A summary of the fitted logistic regression model.
#'
#' @examples
#' logistic_regression(Y ~ X1 + X2, data = data)
#'
#' @export
logistic_regression <- function(formula, data) {
  model <- glm(formula, data = data, family = binomial)
  return(summary(model))
}

#' Poisson Regression Model
#'
#' This function fits a Poisson regression model for count data.
#'
#' @param formula A formula object (e.g., Y ~ X1 + X2).
#' @param data A data frame containing the variables.
#'
#' @return A summary of the fitted Poisson regression model.
#'
#' @examples
#' poisson_regression(Y ~ X1 + X2, data = data)
#'
#' @export
poisson_regression <- function(formula, data) {
  model <- glm(formula, data = data, family = poisson)
  return(summary(model))
}
