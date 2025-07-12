#' Matchit pooled
#' @title assign function -- new dataframe with matched records
#' @description Applies a privacy-preserving transformation to propensity score distances for federated matching. Uses noise injection (normal or k-NN) and ensures safe thresholds per DataSHIELD disclosure settings.
#'
#' @param formula formula, character string specifying the treatment variable
#' @param data character string name of the dataframe
#' @param distance character string name of column in data containing distance measure
#' @param id_name character string for name of ID column in data
#' @param std numeric, standard deviation for normal noise
#' @param k integer, number of nearest neighbors used for k-NN noise
#'
#' @return A dataframe with ID, treatment, and noise-injected distance column
#' @details Disclosure control includes minimum unique values and safe thresholds for standard deviation and k.
#' @author Roy Gusinow
#' @export
matchitDS <- function(formula, data, distance, id_name, k = NULL, std = NULL){

  checkPermissivePrivacyControlLevel(c('permissive'))

  # formula <- eval(parse(text=formula), envir = parent.frame())
  data <- eval(parse(text=data), envir = parent.frame())
  distance <- eval(parse(text=distance), envir = parent.frame())

  thr <- dsBase::listDisclosureSettingsDS()

  if (!is.null(std) && !is.null(k)){
    stop("Cannot use both std and k at the same time", call.=FALSE)
  }
  if (is.null(std) && is.null(k)){
    stop("Must specify either std or k", call.=FALSE)
  }

  if (!is.null(std)){
    nfilter.noise <- as.numeric(thr$nfilter.noise)
    if (std < nfilter.noise){
      stop("Cannot use sd lower than nfilter.noise", call.=FALSE)
    }
  }

  if (!is.null(k)){
    nfilter.kNN <- as.numeric(thr$nfilter.kNN)
    if (k < nfilter.kNN){
      stop("Cannot use k lower than nfilter.kNN for KNN", call.=FALSE)
    }
  }

  treatment <- data[all.vars(formula)[1]]
  id <- data[, id_name]

  if (!is.null(std)){
    if (is.null(std)) {std <- stats::sd(distance)}

    noise_distance <- distance + stats::rnorm(length(distance), mean = 0, sd = std)
  } else if (!is.null(k)){
    distance <- distance$distance
    noise_distance <- distance

    knn_id <- FNN::get.knn(distance, k = k)$nn.index
    for (i in 1:length(noise_distance)){
      noise_distance[i] <- distance[i] + stats::rnorm(1, mean = 0, sd = stats::sd(rbind(distance[knn_id[i, ]]), distance[i]))
    }
  }

  out.frame <- data.frame(ID = id,
                          treatment = treatment,
                          distance = noise_distance)
  colnames(out.frame)[1] <- id_name
  return(out.frame)
}
