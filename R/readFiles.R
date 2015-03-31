#' Read and process mulitple data files

readFiles <- 
	function(files,dp,scans,sranges=list(c(50,1000)),modes=c("p","n"),nCores=2){ # for data collected in both modes
  if(nCores<0){
    pl <- lapply(files,sampProcess,scans=scans,dp=dp,sranges=sranges,modes=modes)
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
    clusterEvalQ(clust,library(OrbiFIEproc))
		pl <- parLapplyLB(clust,files ,fun= sampProcess,scans=scans,dp=dp,sranges=sranges,modes=modes)	 
		# split modes
    pos.neg <- list()
    for (i in 1:length(modes)){
    	pos.neg[[i]] <- lapply(pl,function(x,mode){return(x[mode])},mode=modes[i])
    }  
  	# add in masses to get equal lengths for each sample
		pos.neg <- parLapplyLB(clust,pos.neg,fun=addMasses)	
  	# build  intensity matrix
  	pos.neg <- parLapplyLB(clust,pos.neg,fun=massMat)
  	stopCluster(clust)
  }
  for (i in 1:length(modes)){
  	pos.neg <- lapply(pos.neg,
  										function(x,mode){
  											colnames(x) <- paste(mode,colnames(x),sep="")
  											return(x)
  										},mode=modes[i])
  }
  names(pos.neg) <- modes
  gc()
  return(pos.neg)
}  