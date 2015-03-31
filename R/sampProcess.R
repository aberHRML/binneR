#'  process a single data file

sampProcess <- 
	function(file,scans,dp,sranges,modes){
	aa <- openMSfile(file)
  pl <- peaks(aa) # obtain peak lists
	pl <- combScans(pl,scans,sranges,modes)
	# round and aggregate within scans
	pl <- lapply(pl,
							 function(x,dp){
							 	return(lapply(x,function(y,dp){
							 		# round to dp
							 		y[,"mz"] <- round(y[,"mz"],dp)	
							 		# aggregate bins to give ion totals
							 		y <- aggregate(y[,"intensity"],list(y[,"mz"]),sum)	
							 		return(y)
							 	},dp=dp))
							 	},dp=dp)
	# add zeros for missing bins in each scan 
	pl <- lapply(pl,addMasses)
	pl <- lapply(pl,massMat)
	# average the scans
	pl <- lapply(pl,
							 function(x){
							 	x <- apply(x,2,mean)
							 	return(x)
							 	})
	pl <- lapply(pl,
							 function(x){
							 	x <- data.frame(mz=as.numeric(as.character(names(x))),intensity=x,row.names=NULL)
							 	return(x)
							 })
	names(pl) <- modes
	return(pl)
}