#' Estimating function (score vector)
#'
#' Aggregate function -- returns the unscaled estimating function
#' (score vector), computed as the weighted residuals multiplied
#' by the design matrix. Optionally applies HC2 or HC3 heteroskedasticity
#' corrections using the leverage (diagonal of the hat matrix).
#'
#' @param formula `character`. A formulaula string (e.g. `"Y ~ X1 + X2"`) specifying the model.
#' @param data `character`. The name of the dataframe containing model variables.
#' @param pred_col `character`. The name of the vector of predicted values.
#' @param correction_type `character`. Optional. correction_type of heteroskedasticity correction to apply: `"HC0"` (default), `"HC2"`, or `"HC3"`.
#' @param h_diag `character`. Optional. Name of the vector containing the diagonal elements of the hat matrix. Required for `"HC2"` and `"HC3"`.
#'
#' @return A numeric matrix of residuals multiplied by the design matrix, potentially adjusted for heteroskedasticity.
#'
#' @details
#' This function is used internally to compute robust variance estimates via the estimating function.
#' HC2 and HC3 corrections divide the residuals by leverage-adjusted denominators.
#'
#' @author Roy Gusinow
#' @export
estfunDS <- function(formula,
                     data,
                     pred_col,

                     correction_type = "HC0",
                     h_diag = "h_diag"){

  checkPermissivePrivacyControlLevel(c('permissive'))

  # formula <- eval(parse(text=formula), envir = parent.frame())
  data <- eval(parse(text=data), envir = parent.frame())
  pred_col <- eval(parse(text=pred_col), envir = parent.frame())
  h_diag <- eval(parse(text=h_diag), envir = parent.frame())

  # get evaluation of score vector (diff log likelihood) of
  xmat <- stats::model.matrix(stats::formula(formula), data)

  outcome_var <- all.vars(stats::formula(formula))[1] # get outcome var
  wres <- get_residuals(data[, outcome_var], pred_col) * data[, "weights"] # weights from regressions model!
  rval_unscaled <- wres * xmat

  # HC2/3
  if (correction_type == "HC2" | correction_type == "HC3"){
    if (correction_type == "HC2"){
      rval_unscaled <- rval_unscaled / sqrt(1 - h_diag)
    }else if (correction_type == "HC3"){
      rval_unscaled <- rval_unscaled / (1 - h_diag)
    }

  }

  check_formula_length_disclosure_risk(dim(rval_unscaled)[2])
  check_subset_disclosure_risk(sum(rval_unscaled != 0))

  return(rval_unscaled)
}
