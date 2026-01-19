#' Predict Propensity Scores Using Regression Coefficients
#'
#' Assign function -- calculates the linear prediction (e.g. propensity score)
#' for a fitted regression model based on input coefficients and covariate data.
#'
#' @param formula `character`. A model formulaula (e.g., `"treat ~ age + gender"`) specifying covariates.
#' @param coefficents `numeric`. Vector of regression coefficients (intercept first).
#' @param data `character`. The name of the data frame (server-side object) containing covariates.
#' @param link `character`. The link function used in the original model. One of: `"identity"`, `"logit"`, `"inverse"`, or `"log"`.
#'
#' @return `numeric`. A vector of predicted values (e.g., propensity scores) matching the number of rows in the data.
#'
#' @details
#' This assign function reproduces the predicted values from a federated GLM model,
#' applying the link-inverse transformulaation (e.g., logistic sigmoid for `"logit"` link).
#'
#' @author Roy Gusinow
#' @export
# genPropDS <- function(formula, coefficents, data, link){
#
#   checkPermissivePrivacyControlLevel(c('permissive'))
#
#   formula.vars <- all.vars(formula)
#   data <- eval(parse(text=data), envir = parent.frame())
#
#   # intercept - beta_0
#   estimate <- data[formula.vars[1]]
#   estimate[,] <- coefficents[1]
#   colnames(estimate) <- "distance"
#
#   for (i in 2:length(formula.vars)){
#     estimate <- estimate + data[formula.vars[i]] * coefficents[i]
#   }
#
#   # inverse logit
#   if (link == "identity"){
#
#   }else if (link == "logit"){
#     estimate <- 1 / (1 + exp(-estimate))
#   }else if (link == "inverse"){
#     estimate <- 1 / estimate
#   }else if (link == "log"){
#     estimate <- exp(estimate)
#   }
#   distance <- as.numeric(estimate[["distance"]])
#
#   check_formula_length_disclosure_risk(sum(2:length(formula.vars)))
#   check_subset_disclosure_risk(sum(distance != 0))
#
#   return(distance)
# }

genPropDS <- function(formula, coefficents, data, link){

  checkPermissivePrivacyControlLevel(c('permissive'))

  decode_coef_names <- function(coef_vec) {
    names(coef_vec) <- gsub("___", ":", names(coef_vec), fixed = TRUE)
    coef_vec
  }
  coefficents <- decode_coef_names(coefficents)

  data <- eval(parse(text=data), envir = parent.frame())

  # Build model matrix
  X <- model.matrix(formula, data = data)
  colnames(X)[colnames(X) == "(Intercept)"] <- "Intercept"

  # Align coefficients to model matrix columns
  if (is.null(names(coefficents))) {
    stop("Coefficents must be named.")
  } else {
    beta <- rep(0, ncol(X))
    names(beta) <- colnames(X)
    common <- intersect(names(coefficents), colnames(X))
    beta[common] <- coefficents[common]
    if (any(setdiff(colnames(X), names(coefficents)) != "")) {
      stop("Missing coefficients for some terms.")
    }
  }

  # Linear predictor
  eta <- drop(X %*% beta)

  # Link inverse
  estimate <- switch(
    link,
    identity = eta,
    logit    = plogis(eta),
    inverse  = 1 / eta,
    log      = exp(eta),
    stop("Unsupported link")
  )

  distance <- as.numeric(estimate)

  check_formula_length_disclosure_risk(sum(2:length(all.vars(formula))))
  check_subset_disclosure_risk(sum(distance != 0))

  return(distance)
}

