#' @importFrom purrr map2

binCentrality <- function(mz,intensity,dp = 2){
    intensity <- round(intensity)
    mzs <- unlist(map2(mz,intensity,~{rep(.x,.y)}))
    centrality <- 1 - abs(mean(mzs) - round(mzs,2)[1])/(1*10^-dp/2)
    return(centrality)
}