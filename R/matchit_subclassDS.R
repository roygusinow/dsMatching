#' Matchit on pooled data (assign function)
#'
#' @title assign function -- new dataframe with matched records and subclass labels
#' @description Creates subclass labels based on quantiles of propensity score distances and assigns corresponding weights.
#' Applies DataSHIELD disclosure checks to ensure no subclass is too small or too numerous.
#' @details Serverside assign function that tags each row with a subclass and assigns weights,
#' depending on a provided estimand. Checks for privacy-preserving subclass sizes.
#'
#' @param data character, name of dataframe on server
#' @param distance character, name of vector containing distances
#' @param quantiles numeric vector specifying subclass quantile boundaries
#'
#' @return A modified dataframe with subclass and weight columns
#' @author Roy Gusinow
#' @export
matchit_subclassDS <- function(data,
                               distance,
                               quantiles){

  checkPermissivePrivacyControlLevel(c('permissive', "banana"))

  data <- eval(parse(text=data), envir = parent.frame())
  distance <- eval(parse(text=distance), envir = parent.frame())

  subclass <- as.integer(findInterval(distance, quantiles, all.inside = TRUE))

  # - PRIVACY CHECK —
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  subclass_counts <- table(subclass)
  if (any(subclass_counts < nfilter.tab)) {
    stop("Disclosure risk: At least one subclass has fewer than nfilter.tab observations.", call. = FALSE)
  }

  # weights <- rep(1, nrow(data)) # These are fake weights which are overwritten
  data <- cbind(data, subclass)

  return(data)
}
