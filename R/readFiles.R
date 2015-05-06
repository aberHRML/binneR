#' Read and process mulitple data files
 @export
readFiles <- 
	function(files,dp,scans,sranges=list(c(50,1000)),modes=c("p","n"),nCores=2){ # for data collected in both modes
  if(nCores<0){
    pl <- lapply(files[1:10],sampProcess,scans=scans,dp=dp,sranges=sranges,modes=modes)
    # split modes
    pos.neg <- list()
    for (i in 1:length(modes)){
    	pos.neg[[i]] <- lapply(pl,function(x,mode){return(x[mode])},mode=modes[i])
    }  
    # add in masses to get equal lengths for each sample
		pos.neg <- lapply(pos.neg,addMasses)	
  	# build  intensity matrix
  	pos.neg <- lapply(pos.neg,massMat)	
  }else{
    clust = makeCluster(nCores, type="PSOCK")
    clusterEvalQ(clust, "package:OrbiFIEproc")
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
