#'  process a single data file

sampProcess <- 
	function(file,scans,res,sranges){
	aa <- openMSfile(file)
  pl <- peaks(aa) # obtain peak lists
	pl <- combScans(pl,scans,sranges)
	for (i in 1:length(x)){
				pl.1 <- data.frame(x[i])
				names(pl.1) <- c("mz","intensity")
				pl.1$intensity[pl.1$intensity < cut] <- 0
				pl.1 <- round(pl.1,res)	# round to res
				pl.1 <-pl.1[pl.1$intensity>0,]
				#pl.1 <-pl.1[pl.1$mz>70,] 
				pl.1 <- aggregate(pl.1[,2],list(pl.1[,1]),sum)	# total count for each bin in each scan
				pl.2[i] <- list(pl.1)
			}
			pl.3 <- add.masses(pl.2)	# add zeros for missing bins in each scan 
			masses <- col.masses(pl.3)
			pl.4 <- mass.mat(pl.3)# put scans in to matrix
			pl.4 <- t(pl.4)
			pl.5 <- apply(pl.4,1,mean)	# average the scans
			pl.5 <- data.frame(masses,matrix(pl.5))
			names(pl.5) <- c("mz","intensity")
			pl.6[i] <- list(pl.5)
	
}