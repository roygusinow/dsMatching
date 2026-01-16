#' @noRd
create_unique_ids <- function(n, seed_no = 1, char_len = 10){
  # set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)

  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  return(res)
}

#' @noRd
get_residuals <- function(y, pred){
  # get residuals
  return(y - pred)
}

#' @noRd
manual_cumsum <- function(x, w)
{
  x <- sort(x) # drops NAs
  n <- length(x)
  if(n < 1) {
    return(function(x){NA})
  }
  vals <- unique(x)

  csum <- cumsum(tabulate(match(x, vals)))
  rval <- approxfun(vals, csum,
                    method = "constant", yleft = 0,  yright = max(csum), f = 0,
                    ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  return(rval)
}

#' @noRd
get_domain <- function(min, max, len){
  return(seq(min, max, length.out = len))
}
#' @noRd
compute_rval <- function(vec, min, max, len){
  # compute ecdf step function evaluation before normalising

  domain <- get_domain(min, max, len = len)
  rval <- manual_cumsum(vec)(domain)

  return(rval)
}

#' @noRd
aggr_sum <- function(df, weight_vec = NULL, begin_list = NULL, end_list = NULL, bin_num = 40){

  if (!is.null(weight_vec)){
    weight_vec <- weight_vec
  } else{
    weight_vec <- rep(1, nrow(df))
  }

  # mean
  df_numeric <- df[sapply(df, is.numeric)]
  df_sum <- sapply(weight_vec * df_numeric, sum)
  df_sumsq <- sapply(weight_vec * df_numeric^2, sum)
  N <- sapply(df_numeric, function(x){sum(!is.na(x))})
  sum_weights <- sapply(df_numeric, function(x){sum(weight_vec, na.rm = T)})

  # ecdf
  ecdf_step <- lapply(weight_vec * df_numeric, manual_cumsum)

  # bin_num <- 40
  domain_list <- mapply(seq, begin_list, end_list, length.out = bin_num, SIMPLIFY = F)

  y <- sapply(ecdf_step, mapply, domain_list, SIMPLIFY = F)
  ecdf_diag <- diag(as.matrix(y))

  # change to filter not including 0
  # if (sum(N == 0) > 0) {
  #   stop("Disclosure risk: At least one treatment or control has zero observations", call. = FALSE)
  # }

  out <- list("Sum" = df_sum,
              "Sum Squared" = df_sumsq,
              "N" = N,
              "sum_weights" = sum_weights,

              "eCDF" = ecdf_diag,
              "begin_list" = begin_list,
              "end_list" = end_list,
              "domain" = domain_list
  )
  return(out)
}

#' @noRd
pred <- function(form, coefficents, data, link){
  # manually predict from data and coefficients
  form.vars <- all.vars(stats::formula(form))

  # intercept - beta_0
  estimate <- data[form.vars[1]]
  estimate[,] <- coefficents[1]
  colnames(estimate) <- "distance"

  for (i in 2:length(all.vars(form))){
    estimate <- estimate + data[form.vars[i]] * coefficents[i]
  }

  # inverse logit
  if (link == "identity"){

  }else if (link == "logit"){
    estimate <- 1 / (1 + exp(-estimate))
  }else if (link == "inverse"){
    estimate <- 1 / estimate
  }else if (link == "log"){
    estimate <- exp(estimate)
  }

  return(as.numeric(estimate[["distance"]]))
}

#' @title listDisclosureSettingsDS
#' @description This serverside function is an aggregate function that is called by the
#' ds.listDisclosureSettings
#' @details For more details see the extensive header for ds.listDisclosureSettings
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @noRd
listDisclosureSettingsDS <- function() {
  ds.privacyControlLevel <- getOption("datashield.privacyControlLevel")
  if (is.null(ds.privacyControlLevel)) {
    ds.privacyControlLevel <- getOption("default.datashield.privacyControlLevel")
  }

  nf.tab <- getOption("nfilter.tab")
  if (is.null(nf.tab)) {
    nf.tab <- getOption("default.nfilter.tab")
  }
  nf.subset <- getOption("nfilter.subset")
  if (is.null(nf.subset)) {
    nf.subset <- getOption("default.nfilter.subset")
  }
  nf.glm <- getOption("nfilter.glm")
  if (is.null(nf.glm)) {
    nf.glm <- getOption("default.nfilter.glm")
  }
  nf.string <- getOption("nfilter.string")
  if (is.null(nf.string)) {
    nf.string <- getOption("default.nfilter.string")
  }
  nf.stringShort <- getOption("nfilter.stringShort")
  if (is.null(nf.stringShort)) {
    nf.stringShort <- getOption("default.nfilter.stringShort")
  }
  nf.kNN <- getOption("nfilter.kNN")
  if (is.null(nf.kNN)) {
    nf.kNN <- getOption("default.nfilter.kNN")
  }
  nf.levels.density <- getOption("nfilter.levels.density")
  if (is.null(nf.levels.density)) {
    nf.levels.density <- getOption("default.nfilter.levels.density")
  }
  nf.levels.max <- getOption("nfilter.levels.max")
  if (is.null(nf.levels.max)) {
    nf.levels.max <- getOption("default.nfilter.levels.max")
  }
  nf.noise <- getOption("nfilter.noise")
  if (is.null(nf.noise)) {
    nf.noise <- getOption("default.nfilter.noise")
  }
  nfilter.privacy.old <- getOption("datashield.privacyLevel")

  return(list(
    datashield.privacyControlLevel = ds.privacyControlLevel, nfilter.tab = nf.tab, nfilter.subset = nf.subset,
    nfilter.glm = nf.glm, nfilter.string = nf.string,
    nfilter.stringShort = nf.stringShort, nfilter.kNN = nf.kNN, nfilter.levels.density = nf.levels.density,
    nfilter.levels.max = nf.levels.max, nfilter.noise = nf.noise, nfilter.privacy.old = nfilter.privacy.old
  ))
}

#' @noRd
check_subset_disclosure_risk <- function(val) {

  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.subset <- as.numeric(thr$nfilter.subset)

  if (val < nfilter.subset) {
    stop("Disclosure risk: too many non-zero entries in vector used")
  }

}

#' @noRd
check_formula_length_disclosure_risk <- function(val) {

  if (val == 1) {
    stop("FAILED: Model formula cannot have only one dependant variable", call. = FALSE)
  }

}

#'
#' @title checkPermissivePrivacyControlLevel
#' @description This serverside function check that the server is running in "permissive" privacy control level.
#' @details Tests whether the R option "datashield.privacyControlLevel" is set to "permissive", if it isn't
#' will cause a call to stop() with the message "BLOCKED: The server is running in 'non-permissive' mode which
#' has caused this method to be blocked".
#' @param privacyControlLevels is a vector of strings which contains the privacy control level names which are permitted by the calling method.
#' @importFrom cli cli_abort
#' @author Wheater, Dr SM., DataSHIELD Team.
#' @return Returns an error if the method is not permitted; otherwise, no value is returned.
#' @noRd
checkPermissivePrivacyControlLevel <- function(privacyControlLevels){
  disclosureSettings <- listDisclosureSettingsDS()
  if (is.null(disclosureSettings) || is.null(disclosureSettings$datashield.privacyControlLevel) ||
      (! any(disclosureSettings$datashield.privacyControlLevel %in% privacyControlLevels))) {
    cli_abort("BLOCKED: The server is running in 'non-permissive' mode which has caused this method
              to be blocked", call. = TRUE)
  }

  invisible()
}
