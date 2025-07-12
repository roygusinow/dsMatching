#' Match Summary by Subclass
#'
#' Aggregate function — summarizes balance across subclasses created during matching.
#'
#' @param object `character`. The name of the server-side data frame with subclass information.
#' @param bin_num `numeric`. Number of bins to use when computing summary statistics (passed to `match_summaryDS`).
#' @param treatment `character`. The name of the treatment indicator variable.
#' @param dframe `character`. Name of the data frame used for balance checking.
#' @param weights `character`. Name of the weight variable or weighting vector used in balance summary.
#'
#' @return A named list of balance summaries, one per subclass, each produced by `match_summaryDS`.
#'
#' @details
#' This function splits the provided matched data by `subclass` and applies
#' `match_summaryDS()` to each group to evaluate covariate balance.
#'
#' @author Roy Gusinow
#'
#' @export
match_summary_subclassDS <- function(object,
                                      bin_num,
                                      treatment,
                                      dframe,
                                      weights){

  checkPermissivePrivacyControlLevel(c('permissive', "banana"))

  # convert into list
  object <- eval(parse(text=object), envir = parent.frame())
  object_list <- split(object, object[, "subclass"])

  # - PRIVACY CHECK —
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  subclass_counts <- table(object$subclass)
  if (any(subclass_counts < nfilter.tab)) {
    stop("Disclosure risk: At least one subclass has fewer than nfilter.tab observations.", call. = FALSE)
  }

  out_list <- lapply(object_list,
                     match_summaryDS,
                     bin_num,
                     treatment,
                     dframe,
                     weights)

  return(out_list)
}

