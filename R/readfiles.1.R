readfiles.1 <- 
	function(files,res=2,scans=8:20,cut=0,mode="n"){  # for data collected in only one mode
    library(mzR)
	pl <- NULL
	p <- NULL
	for (i in 1:length(files)){
	print(files[i])
		flush.console()
		aa <- openMSfile(files[i])
		pl <- peaks(aa) # obtain peak lists
		pl.p <- pl[scans]  # range of scans to use 
		pl.p.1 <- samp.process(pl.p,res,cut)
		p[i] <- pl.p.1
	}
	p <- add.masses(p)	# add in masses to get equal lengths for each sample
	p <- mass.mat(p)		# build  intensity matrix 
	colnames(p) <- paste(mode,colnames(p),sep="")
	return(p)
}