#' binParameters
#' @description selection of parameters to use for spectral binning.
#' @param scans numeric vector containing the scan indexes to use for binning
#' @param modes character vector denoting the order and names of the modes
#' @param sranges list of vectors containing the ranges of the scan events present
#' @param cls the column of class labels to use for aggregating accurate mass data. Defaults to NULL where accurate mass data will be averaged accross all samples
#' @param nCores the number of cores to use for parallel processing
#' @param clusterType the type of cluster to use for parallel processing
#' @importFrom parallel detectCores
#' @export

binParameters <- function(scans = 5:12, modes = c('n','p'), sranges = list(c(70,1000)), cls = character(), nCores = detectCores(), clusterType = 'FORK'){
    new('BinParameters',
        scans = scans,
        modes = modes,
        sranges = sranges,
        cls = cls,
        nCores = nCores,
        clusterType = clusterType
        )
}