#' Match Summary for Covariate Balance
#'
#' Aggregate function — computes summary statistics and empirical CDFs for matched treatment and control groups.
#'
#' @param object `character` or `data.frame`. The name of the server-side matched object or the object itself.
#' @param bin_num `numeric`. Number of bins for computing the empirical CDF.
#' @param treatment `character`. Name of the treatment variable (must be binary).
#' @param dframe `character`. A comma-separated string encoding the variable names and their ranges, used for binning.
#' @param weights `logical`. Whether to use observation weights when computing summaries.
#'
#' @return A list containing summary statistics (`Sum`, `Sum Squared`, `N`, and `sum_weights`) and
#' empirical CDFs for both treatment and control groups.
#'
#' @details
#' The input `dframe` encodes a matrix with:
#' - Variable names in every third element
#' - Corresponding min/max range values interleaved in between
#'
#' The output object is a nested list with separate summaries for control and treatment groups.
#'
#'
#' @author Roy Gusinow
#' @export
match_summaryDS <- function(object,
                            bin_num,
                            treatment,
                            dframe,
                            weights){

  checkPermissivePrivacyControlLevel(c('permissive', "banana"))

  if (is.character(object)){
    object <- eval(parse(text=object), envir = parent.frame())
  }

  dframe_vec <- unlist(strsplit(dframe, split=","))
  colnames_dframe <- dframe_vec[seq(1, length(dframe_vec), 3)]
  range_dframe <- dframe_vec[! dframe_vec %in% colnames_dframe]

  range_dframe.n <- matrix(as.numeric(range_dframe), nrow = 2)

  begin_list <- as.list(range_dframe.n[1,])
  names(begin_list) <- colnames_dframe
  end_list <- as.list(range_dframe.n[2,])
  names(end_list) <- colnames_dframe

  df_control <- object[object[, treatment] == 0, colnames_dframe]
  df_treat <- object[object[, treatment] == 1, colnames_dframe]

  if (weights){
    out <- list("Control" = aggr_sum(df_control, weight_vec = object[object[, treatment] == 0, "weights"], begin_list = begin_list, end_list = end_list, bin_num = bin_num),
                "Treated" = aggr_sum(df_treat, weight_vec = object[object[, treatment] == 1, "weights"], begin_list = begin_list, end_list = end_list, bin_num = bin_num))
  }else{
    out <- list("Control" = aggr_sum(df_control, weight_vec = NULL, begin_list = begin_list, end_list = end_list, bin_num = bin_num),
                "Treated" = aggr_sum(df_treat, weight_vec = NULL, begin_list = begin_list, end_list = end_list, bin_num = bin_num))
  }

  return(out)
}
