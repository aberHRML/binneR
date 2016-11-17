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
#' @examples 
#' res <- readFiles(list.files(system.file('mzXML',package = 'binneR'),
#'                  full.names=TRUE),dp = 2,scans = 6:17)

readFiles <- function(files,dp, scans, sranges = list(c(50,1000)), modes = c("n","p"), nCores = 1){ # for data collected in both modes
	clust = makeCluster(nCores, type = "PSOCK") 
	pl <- parLapplyLB(clust,files ,fun = sampProcess,scans = scans,dp = dp,sranges = sranges,modes = modes)	 
	# split modes
	pos.neg <- list()
	for (i in 1:length(modes)) {
		pos.neg[[i]] <- lapply(pl,function(x,mode){return(x[mode])},mode = modes[i])
	}  
	pl <- NULL
	gc()
	# add in masses to get equal lengths for each sample
	pos.neg <- parLapplyLB(clust,pos.neg,fun = addMasses)	
	gc()
	# build  intensity matrix
	pos.neg <- parLapplyLB(clust,pos.neg,fun = massMat)
	stopCluster(clust)
	for (i in 1:length(modes)) {
		colnames(pos.neg[[i]]) <- paste(modes[i],colnames(pos.neg[[i]]),sep = "")
	}
	names(pos.neg) <- modes
	gc()
	return(pos.neg)
}  
