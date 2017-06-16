#' Read and process mulitple data files
#' @name readFiles
#' @description Apply spectral binning on multiple data files.
#' @param files A vector of converted data file paths
#' @param dp An integer denoting the number of decimal places for spectral binning
#' @param scans A vector of scan numbers that should be retrieved 
#' @param sranges A list of vectors containing the scan events present.
#' @param modes A vector of strings denoting mode names including the order in which the scan events occur.
#' @param nCores The number of cores on which to parallel process.
#' @return A list containing peak lists for the relevant scans with combined scan ranges for each present mode in the data file. 
#' @author Jasen Finch
#' @export
#' @importFrom parallel makeCluster parLapplyLB stopCluster
#' @importFrom dplyr bind_rows 
#' @importFrom tidyr spread
#' @importFrom plyr dlply
#' @examples 
#' res <- readFiles(list.files(system.file('mzXML',package = 'binneR'),
#'                  full.names=TRUE),dp = 2,scans = 6:17)

readFiles <- function(files,dp, scans, sranges = list(c(50,1000)), modes = c("n","p"), nCores = 1){ # for data collected in both modes
	clust = makeCluster(nCores, type = "PSOCK") 
	pl <- parLapplyLB(clust,files ,fun = sampProcess,scans = scans,dp = dp,sranges = sranges,modes = modes)	 
	stopCluster(clust)
	names(pl) <- files
	pl <- bind_rows(pl,.id = 'File')
	pl$mz <- paste(pl$Mode,pl$mz,sep = '')
	# split modes
	pl <- dlply(pl, 'Mode', identity)
	# build  intensity matrix
	pl <- lapply(pl,function(x){
		x <- spread(x,key = 'mz',value = 'intensity',fill = 0)
		x$File <- NULL
		x$Mode <- NULL
		x <- as.matrix(x)
		return(x)
	})
	return(pl)
}  
