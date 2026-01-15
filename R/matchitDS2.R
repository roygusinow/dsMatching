#' Matchit postprocessing (pooled)
#' @title assign function -- new dataframe with matched records
#' @description Integrates matched ID records with distance and weights. Returns subset of matched individuals only.
#'
#' @param data character string name of the dataframe
#' @param ids vector of matched IDs
#' @param distances vector of distances for matched records
#' @param weights vector of weights for matched records
#' @param id_name character string, name of the ID column in `data`
#'
#' @return Subsetted dataframe with matched records and distance/weights columns filled
#' @details Applies disclosure checks to prevent returning too few matched records (using `nfilter.tab`)
#' @author Roy Gusinow
#'
#' @export
matchitDS2 <- function(data,
                       ids,
                       distances,
                       weights,
                       subclass,
                       id_name){

  checkPermissivePrivacyControlLevel(c('permissive'))

  data <- eval(parse(text=data), envir = parent.frame())

  data$distance <- rep(0, dim(data)[1])
  data$weights <- rep(0, dim(data)[1])
  data$subclass <- rep(0, dim(data)[1])
  log_m <- rep(FALSE, dim(data)[1])

  # look in each server and see which records have been matched
  # add the distance and weight aspects to that entry
  common_id <- intersect(data[, id_name], ids)
  for (id in common_id){
    server_id <- which(data[, id_name] == id)
    match_id <- which(ids == id)

    data$distance[server_id] <- distances[match_id]
    data$weights[server_id] <- weights[match_id]
    data$subclass[server_id] <- subclass[match_id]

    log_m[server_id] <- TRUE
  }

  # security checks
  check_subset_disclosure_risk(nrow(data[log_m,]))

  return(data[log_m,])
}
