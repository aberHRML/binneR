#' @importFrom stats sd

binPurity <- function(mz,intensity,dp = 2){
  intensity <- ceiling(intensity)
  if (sum(intensity) > 1) {
    mzs <- unlist(map2(mz,intensity,~{rep(.x,.y)}))
    purity <- 1 - (sd(mzs)/(1*10^-dp))
  } else {
    purity <- 1
  }
  return(purity)
}