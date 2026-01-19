#' Matchit pooled
#' @title assign function -- new dataframe with matched records
#' @description Applies a privacy-preserving transformation to propensity score distances for federated matching. Uses noise injection (normal or k-NN) and ensures safe thresholds per DataSHIELD disclosure settings.
#'
#' @param formula formula, character string specifying the treatment variable
#' @param data character string name of the dataframe
#' @param distance character string name of column in data containing distance measure
#' @param id_name character string for name of ID column in data
#'
#' @return A dataframe with ID, treatment, and noise-injected distance column
#' @details Disclosure control includes minimum unique values and safe thresholds for standard deviation and k.
#' @author Roy Gusinow
#' @export
matchitDS <- function(formula, data, distance, id_name){

  checkPermissivePrivacyControlLevel(c('permissive', "banana"))

  data <- eval(parse(text=data), envir = parent.frame())
  distance <- eval(parse(text=distance), envir = parent.frame())

  thr <- dsBase::listDisclosureSettingsDS()

  if (is.null(thr$default.privacy_budget)){
    # warning("No default privacy budget set at the server side. Setting budget to 10^5")
    # thr$default.privacy_budget <- 10^5
    thr$default.privacy_budget <- 10^20 # infinte
  }

  treatment <- data[all.vars(formula)[1]]
  id <- data[, id_name]

  b <- 1 / as.numeric(thr$default.privacy_budget)
  noise_distance <- distance + VGAM::rlaplace(n = length(distance), loc = 0, scale = b)

  out.frame <- data.frame(ID = id,
                          treatment = treatment,
                          distance = noise_distance)
  colnames(out.frame)[1] <- id_name
  return(out.frame)
}
