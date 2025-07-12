#' Estimate dispersion from weighted residuals (Aggregate function)
#'
#' @title aggregate function -- create dispersion for clustered error estimate
#' @description Computes a dispersion estimate using weighted residuals from a regression model.
#' For GLM families like Poisson or Binomial, the dispersion is set to 1. For others, it computes
#' the sum of squared residuals.
#'
#' @param formula character string formulaula (e.g., "y ~ x1 + x2")
#' @param data character string name of the dataframe
#' @param pred_col character string name of column with predictions
#' @param family character GLM family (e.g., "poisson", "binomial", "gaussian")
#'
#' @return numeric dispersion value (scalar)
#' @details Aggregate function that returns a non-disclosive dispersion measure. A disclosure control check is applied to ensure that the number of residuals exceeds `nfilter.tab`.
#' @author Roy Gusinow
#'
#' @export
dispersionDS <- function(formula,
                         data,
                         pred_col,
                         family){

  checkPermissivePrivacyControlLevel(c('permissive', "banana", "avocado", "non-permissive"))

  # --- Privacy: Capture nfilter settings
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)

  # formula <- eval(parse(text=formula), envir = parent.frame())
  data <- eval(parse(text=data), envir = parent.frame())
  pred_col <- eval(parse(text=pred_col), envir = parent.frame())

  xmat <- stats::model.matrix(stats::formula(formula), data)

  outcome_var <- all.vars(formula(formula))[1] # get outcome var

  wres <- get_residuals(data[, outcome_var], pred_col) * data[, "weights"] # weights from regressions model!

  # security checks
  check_subset_disclosure_risk(length(wres[wres != 0]))

  dispersion <- if(family %in% c("poisson", "binomial", "Negative Binomial")) 1
  else sum(wres^2, na.rm = TRUE)

  return(dispersion)

}
