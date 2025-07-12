#' @title Assign Regression-Based Weights Using Subclass Membership (Assign Function)
#'
#' @description This server-side assign function calculates and returns a vector of weights derived from subclass-specific treatment effect estimates, typically used after federated subclass matching in a GLM context.
#'
#' @param data character. The name of the data frame on the server containing treatment and subclass information.
#' @param weights_list list. A named list of subclass weights (e.g., from pooled ATT/ATC estimates), where names are prefixed with "T" or "C" for treated/control groups.
#' @param treatment character. The name of the binary treatment variable (0/1) in the data.
#' @param estimand character. One of `"ATT"`, `"ATC"`, or `"ATE"` to indicate the causal estimand used for weighting.
#'
#' @return A numeric vector of weights computed for each row in the data.
#'
#' @details
#' This function computes per-subject weights based on subclass assignment and treatment status to support inverse probability weighting in federated analysis workflows.
#'
#' @author Roy Gusinow
#'
#' @export
assign_weights_subclassDS <- function(data, weights_list, treatment, estimand){

  checkPermissivePrivacyControlLevel(c('permissive', "banana"))

  data <- eval(parse(text=data), envir = parent.frame())
  weights <- rep(1, nrow(data))
  if (estimand == "ATT"){
    weights <- ifelse(
      data[[treatment]] == 1,
      1,
      sapply(data$subclass, function(sc) {
        weights_list[[paste0("T", sc)]]
      })
    )
  } else if (estimand == "ATC") {
    weights <- ifelse(
      data[[treatment]] == 0,
      1,
      sapply(data$subclass, function(sc) {
        weights_list[[paste0("C", sc)]]
      })
    )
  } else if (estimand == "ATE") {
    weights <- mapply(
      function(treat, sb) {
        ifelse(
          treat == 1,
          weights_list[[paste0("T", sb)]],
          weights_list[[paste0("C", sb)]]
        )
      },
      data[[treatment]], data$subclass, SIMPLIFY = TRUE
    )

  }

  check_subset_disclosure_risk(length(weights[weights != 0]))

  return(weights)
}
