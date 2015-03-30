#' Combine scan ranges and retrieve given scans

combScans <- 
	function(x,scans,sranges){
	seq.p <- seq(2,length(pl),2)
	seq.n <- seq(1,length(pl),2)

  
	Path <- "G:/Data/150301-MAIN-IMP-P/mzXML_centroid"
	files <- list.files(Path,full.names = T)
	sranges <- list(c(55,225),c(225,635),c(635,1700))
	scans <- 3:8


	aa <- openMSfile(files[1])
	pl <- peaks(aa) # obtain peak lists
	a <- header(aa)
	# separate positive and negative scans
	pat <- c(rep("p",length(sranges)),rep("n",length(sranges)))
	pat <- rep(pat,length(pl)/length(pat))
	pos.neg <- list(Positive_Mode=pl[which(pat=="p")],Negative_Mode=pl[which(pat=="n")]) 
	# separate scan ranges and keep only relevant scans
	pat.1 <- unlist(lapply(seq(1,length(neg)/length(sranges)),rep,times=3))
	pos.neg <- lapply(pos.neg,
										function(x,pat){
											y <- list()
											for (i in unique(pat)){
												y[[i]] <- x[which(pat==i)]
											}
											return(y)
										},pat=pat.1)
	pos.neg <- lapply(pos.neg,
										function(x,scans){
											return(x[scans])
										},scans=scans)
	# combine scan ranges into single lists, boundary always equal to upper limit
	pos.neg <- lapply(pos.neg,
										function(x,sranges){lapply(x,
																							 function(y,sranges){lapply(y,
																																					 function(z,sranges){
																																					 		print(class(z))
																																					 		w <- list()
																																							for (j in 1:length(sranges)){
																																								w[[j]] <- z[which(z[,1]>srange[[j]][1] & z[,1] <= srange[[j]][2]),]
																																							}
																																					 },sranges=sranges)
												
											
											},sranges=sranges)
										},sranges=sranges)
}

a <- pos.neg[[1]]
for (i in 1:length(a)){
	z <- a[[i]] 
	w <- list()

}
																	
	