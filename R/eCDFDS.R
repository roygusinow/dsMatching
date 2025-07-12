#' eCDF vector for a numeric variable
#'
#' @title aggregate function -- empirical cumulative distribution function
#' @description Computes the eCDF over a user-defined domain range and resolution, avoiding disclosure by capping the resolution and verifying data length.
#'
#' @param object character string specifying the name of the vector on the server
#' @param min numeric minimum of the domain
#' @param max numeric maximum of the domain
#' @param len integer length of the output eCDF vector (number of points in domain)
#' @param weights optional; currently unused
#'
#' @return A numeric vector of cumulative probabilities at each point in the domain
#' @details This is a server-side aggregate function for drawing a non-disclosive empirical CDF. Resolution is limited to reduce disclosure risk. Can be paired with a client function that plots this result.
#' @author Roy Gusinow
#'
#' @export
eCDFDS <- function(object,
                   min,
                   max,
                   len,
                   weights){

  checkPermissivePrivacyControlLevel(c('permissive', "banana"))

  # get rval of the ecdf
  object <- eval(parse(text=object), envir = parent.frame())
  rval <- compute_rval(object, min, max, len = len)

  check_subset_disclosure_risk(sum(rval != 0))

  return(rval)
}
