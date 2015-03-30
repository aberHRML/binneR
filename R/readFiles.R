#' Read and process mulitple data files

readFiles <- 
	function(files,sranges=list(c(50,1000)),dp=2,scans=8:20,nCores=2){ # for data collected in both modes
  if(nCores<0){
    pos.neg <- lapply(files,sampProcess,scans=scans,dp=dp)
    # add in masses to get equal lengths for each sample
		pos.neg <- lapply(pos.neg,addMasses)	
  	# build  intensity matrix
  	pos.neg <- lapply(pos.neg,massMat)	
  }else{
    clust = makeCluster(nCores, type="PSOCK")
		pos.neg <- parLapplyLB(clust,files ,fun= sampProcess,scans=scans,res=res,cut=cut)	 
  	# add in masses to get equal lengths for each sample
		pos.neg <- parLapplyLB(clust,pos.neg,fun=addMasses)	
  	# build  intensity matrix
  	pos.neg <- parLapplyLB(clust,pos.neg,fun=massMat)
  	stopCluster(clust)
  }
  colnames(pos.neg[[1]]) <- paste("p",colnames(pos.neg[[1]]),sep="")
  colnames(pos.neg[[2]]) <- paste("n",colnames(pos.neg[[2]]),sep="")
  names(pos.neg) <- c("Positive_Mode","Negative_Mode")
  return(pos.neg)
}  