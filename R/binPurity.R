#' @importFrom stats sd

binPurity <- function(mz,intensity,dp = 2){
    intensity <- round(intensity)
    mzs <- unlist(map2(mz,intensity,~{rep(.x,.y)}))
    purity <- 1 - (sd(mzs)/(1*10^-dp))
    return(purity)
}