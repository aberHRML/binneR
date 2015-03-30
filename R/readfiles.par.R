readfiles.par <- 
	function(files,srce,res=2,scans=8:20,cut=0,par.mode,nCores=1,Path,DF){ # for data collected in both modes
  suppressPackageStartupMessages(library(mzR))
  neg <- NULL
  pos <- NULL
  time1 <- FIEmspro:::timer_start()
  files <- lapply(files,function(x){return(x)})
  if(!(par.mode)){
    pos.neg <- lapply(files,process.ind,scans.1=scans,res.1=res,cut.1=cut,srce.1=srce)
  }else{
	library(parallel)
    			clust = makeCluster(nCores, type="PSOCK")
		pos.neg <- clusterApplyLB(clust,files ,fun= process.ind,scans.1=scans,res.1=res,cut.1=cut,srce.1=srce)
    	stopCluster(clust)	
  }
  for(i in 1:length(pos.neg)){
    samp <- pos.neg[[i]] 
    pos[i] <- samp[[1]]
    neg[i] <- samp[[2]]
  }
  pos <- add.masses(pos)	# add in masses to get equal lengths for each sample
  pos <- mass.mat(pos)		# build  intensity matrix 
  neg <- add.masses(neg)
  neg <- mass.mat(neg)
  colnames(pos) <- paste("p",colnames(pos),sep="")
  colnames(neg) <- paste("n",colnames(neg),sep="")
  dat <- list(pos,neg)
  names(dat) <- c("pos","neg")
  cat(as.character(res),'dp done in ',FIEmspro:::timer_end(time1)$dt,"\n",sep="")
  cat(as.character(res),'dp done in ',FIEmspro:::timer_end(time1)$dt,"\n",sep="",file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
  return(dat)
}  