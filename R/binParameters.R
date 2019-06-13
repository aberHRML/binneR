#' binParameters
#' @description selection of parameters to use for spectral binning.
#' @param scans numeric vector containing the scan indexes to use for binning
#' @param modes character vector denoting the order and names of the modes
#' @param sranges list of vectors containing the ranges of the scan events present
#' @param cls the column of class labels to use for aggregating accurate mass data. Defaults to NULL where accurate mass data will be averaged accross all samples
#' @param nCores the number of cores to use for parallel processing
#' @param clusterType the type of cluster to use for parallel processing
#' @examples 
#' p <- binParameters(scans = 6:17,nCores = 2,clusterType = 'PSOCK')
#' 
#' ## Example using multiple overlapping scan ranges
#' p <- binParameters(scans = 6:17,sranges = list(c(55,280),c(270,1200)))
#' 
#' @importFrom parallel detectCores
#' @export

binParameters <- function(scans = 5:12, modes = c('n','p'), sranges = list(c(70,1000)), cls = character(), nCores = detectCores() * 0.75, clusterType = 'FORK'){
   p <- new('BinParameters',
        scans = scans,
        modes = modes,
        sranges = sranges,
        cls = cls,
        nCores = nCores,
        clusterType = clusterType
        )
   
   if (.Platform$OS.type == 'windows') {
       p@clusterType <- 'PSOCK'    
   }
   
   return(p)
}
