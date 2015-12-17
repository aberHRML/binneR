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
readFiles <- function(files,dp,scans,sranges=list(c(50,1000)),modes=c("p","n"),nCores=2){ # for data collected in both modes
  if(nCores<2){
    pl <- lapply(files,sampProcess,scans=scans,dp=dp,sranges=sranges,modes=modes)
    # split modes
    pos.neg <- list()
    for (i in 1:length(modes)){
    	pos.neg[[i]] <- lapply(pl,function(x,mode){return(x[mode])},mode=modes[i])
    	gc()
    }  
    # add in masses to get equal lengths for each sample
    for (i in 1:length(pos.neg)){
			pos.neg[[i]] <- addMasses(pos.neg[[i]])
			gc()
    }
  	# build  intensity matrix
    for (i in 1:length(pos.neg)){
  		pos.neg[[i]] <- massMat(pos.neg[[i]])	
  		gc()
    }
  }else{
    clust = makeCluster(nCores, type="PSOCK") 
    clusterExport(clust,c(ls("package:OrbiFIEproc"),ls("package:mzR"),ls("package:Rcpp"),ls("package:plyr")))
		pl <- parLapplyLB(clust,files ,fun= sampProcess,scans=scans,dp=dp,sranges=sranges,modes=modes)	 
		# split modes
    pos.neg <- list()
    for (i in 1:length(modes)){
    	pos.neg[[i]] <- lapply(pl,function(x,mode){return(x[mode])},mode=modes[i])
    }  
		pl <- NULL
		gc()
  	# add in masses to get equal lengths for each sample
		pos.neg <- parLapplyLB(clust,pos.neg,fun=addMasses)	
		gc()
  	# build  intensity matrix
  	pos.neg <- parLapplyLB(clust,pos.neg,fun=massMat)
  	stopCluster(clust)
  }
  for (i in 1:length(modes)){
  	colnames(pos.neg[[i]]) <- paste(modes[i],colnames(pos.neg[[i]]),sep="")
  }
  names(pos.neg) <- modes
  gc()
  return(pos.neg)
}  
