#' @title Assign a Unique Sample ID to Each Row of a Data Frame (Assign Function)
#' @description This server-side assign function generates a column of unique alphanumeric sample IDs and assigns them to each row of the specified data frame.
#'
#' @param object character. The name of the data frame on the server to which the IDs will be assigned.
#' @param id_name character. The name of the new column to store the unique IDs.
#'
#' @return A modified data frame with a new column containing unique IDs. The object is overwritten server-side.
#'
#' @details
#' This function is useful for generating unique row identifiers in federated settings where row-level tracking is needed but data must remain anonymized.
#'
#' @author Roy Gusinow
#' @export
assign_IDDS <- function(object, id_name){

  checkPermissivePrivacyControlLevel(c('permissive'))

  object <- eval(parse(text=object), envir = parent.frame())
  object[id_name] <- create_unique_ids(dim(object)[1])

  return(object)
}
