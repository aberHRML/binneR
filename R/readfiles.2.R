readfiles.2 <- 
	function(files,res=2,scans=8:20,cut=0){ # for data collected in both modes
  library(mzR)
	pl <-  NULL
	pl.p <- NULL
	pl.n <- NULL
	pos.s <- NULL
	neg.s <- NULL
	neg <- NULL
	pos <- NULL

	for (i in 1:length(files)){
		print(files[i])
		flush.console()
		aa <- openMSfile(files[i])
		pl <- peaks(aa) # obtain peak lists
		seq.p <- seq(2,length(pl),2)
		seq.n <- seq(1,length(pl),2)
		pos.s <- pl[seq.p]  # separate positive and negative scans 
		neg.s <- pl[seq.n]
		pl.p <- pos.s[scans]  # range of scans to use 
		pl.n <- neg.s[scans]
		print("pos")
		flush.console()
		pl.p.1 <- samp.process(pl.p,res,cut)
		print("neg")
		flush.console()
		pl.n.1 <- samp.process(pl.n,res,cut)
		pos[i] <- pl.p.1
		neg[i] <- pl.n.1
	}
	pos <- add.masses(pos)	# add in masses to get equal lengths for each sample
	pos <- mass.mat(pos)		# build  intensity matrix 
	neg <- add.masses(neg)
	neg <- mass.mat(neg)
	colnames(pos) <- paste("p",colnames(pos),sep="")
	colnames(neg) <- paste("n",colnames(neg),sep="")
	dat <- list(pos,neg)
	names(dat) <- c("pos","neg")
	return(dat)
}  