#' Read and process mulitple data files
#' @name readFiles
#' @description Apply spectral binning on multiple data files.
#' @param files A vector of converted data file paths
#' @param dp An integer denoting the number of decimal places for spectral binning
#' @param scans A vector of scan numbers that should be retrieved 
#' @param nCores The number of cores on which to parallel process.
#' @param clusterType the type of cluster to use for parallel processing
#' @return A list containing peak lists for the relevant scans with combined scan ranges for each present mode in the data file. 
#' @author Jasen Finch
#' @export
#' @importFrom parallel makeCluster parLapplyLB stopCluster
#' @importFrom dplyr bind_rows 
#' @importFrom tidyr spread
#' @examples 
#' res <- readFiles(metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1],dp = 2,scans = 6:17)

readFiles <- function(files,dp, scans, nCores = 1, clusterType = detectClusterType()){ # for data collected in both modes
	clust = makeCluster(nCores, type = clusterType) 
	pl <- parLapplyLB(clust,files ,fun = sampProcess,scans = scans,dp = dp) %>%
		set_names(files) %>%
		bind_rows(.id = 'file') %>%
		mutate(mz = str_c(polarity,mz)) %>%
		split(.$polarity) %>%
		parLapplyLB(cl = clust,function(x){
		x <- spread(x,key = 'mz',value = 'intensity',fill = 0) %>%
			ungroup() %>%
			select(-file,-polarity)
		return(x)
	})
	stopCluster(clust)
	return(pl)
}  
