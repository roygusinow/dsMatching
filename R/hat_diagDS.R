#' Compute Hat Diagonal (Leverage Scores) via Mahalanobis Distance
#'
#' Assign function — calculates the hat diagonals (leverages) using
#' Mahalanobis distance between rows of the design matrix and a global mean/covariance.
#'
#' @param formula `character`. Model formula (e.g., `"~ age + bmi + gender"`).
#' @param data `character`. Name of server-side data frame.
#' @param global_cov_mat `numeric`. Covariance matrix (as a vector in column-major order) of the covariates across all studies.
#' @param global_avg_vec `numeric`. Vector of means for the covariates across all studies.
#' @param global_n `numeric`. Total number of individuals across all studies.
#'
#' @return `numeric`. A vector of leverage scores (hat diagonals) for each row in the data.
#'
#' @details
#' This function computes a Mahalanobis-based approximation to the hat matrix diagonals,
#' useful for robust variance estimation in federated models. The approximation uses a
#' global covariance matrix and mean vector computed across all studies.
#'
#' @author Roy Gusinow
#' @export
hat_diagDS <- function(formula,
                       data,

                       global_cov_mat,
                       global_avg_vec,
                       global_n){

  checkPermissivePrivacyControlLevel(c('permissive'))

  formula.vars <- all.vars(formula)

  dep_vars <- formula.vars[2:length(formula.vars)] # extract dependent vars
  data <- eval(parse(text=data), envir = parent.frame())
  global_cov_mat <- matrix(global_cov_mat, nrow = dim(data)[2])
  rownames(global_cov_mat) <- colnames(data); colnames(global_cov_mat) <- colnames(data)

  xmat <- stats::model.matrix(formula(formula), data)
  # drop intercept and other vars
  xmat <- xmat[, dep_vars]
  global_cov_mat <- global_cov_mat[, dep_vars]
  global_cov_mat <- t(t(global_cov_mat)[, dep_vars])
  global_avg_vec <- global_avg_vec[dep_vars]

  # get hat diag using mahalanobis
  vec_diff <- xmat - rep(global_avg_vec, each = nrow(xmat))
  m_D2 <- vec_diff %*% solve(global_cov_mat) %*% t(vec_diff) # mahalanobis distance

  H <- 1 / global_n + m_D2 / (global_n - 1)
  diag_H <- diag(H)

  # security checks
  check_formula_length_disclosure_risk(length(dep_vars))
  check_subset_disclosure_risk(sum(diag_H != 0))

  return(diag_H)
}
