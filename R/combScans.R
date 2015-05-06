#' Combine scan ranges and retrieve given scans
#' @export
combScans <- 
	function(x,scans,sranges,modes){
	# separate scans based on modes
	pat <- unlist(lapply(modes,
											 function(x,sranges){
											 	rep(x,length(sranges))
											 },sranges=sranges))
	pat <- rep(pat,length(x)/length(pat))
	pos.neg <- lapply(modes,
										function(y,x,pat){
											x <- x[which(pat==y)]
											return(x)
										},pat=pat,x=x)
	# separate scan ranges and keep only relevant scans
	pat.1 <- unlist(lapply(seq(1,length(pos.neg[[1]])/length(sranges)),rep,times=3))
	pos.neg <- lapply(pos.neg,
										function(x,pattern){
											y <- list()
											for (i in unique(pattern)){
												y[[i]] <- x[which(pattern==i)]
											}
											return(y)
										},pattern=pat.1)
	pos.neg <- lapply(pos.neg,
										function(x,scans){
											return(x[scans])
										},scans=scans)
	# combine scan ranges into single lists, boundary always equal to upper limit
	pos.neg <- lapply(pos.neg,
										function(x,sranges){return(lapply(x,
																							 function(y,sranges){ 
																							 	for (i in 1:length(y)){
																							 		z <- y[[i]]
																									y[[i]] <- z[which(z[,1]>sranges[[i]][1] & z[,1] <= sranges[[i]][2]),]
																							 }
																							 y <- ldply(y,data.frame)
																							 names(y) <- c("mz","intensity")
																							 return(y)
																							},sranges=sranges)	
											)},sranges=sranges)
	names(pos.neg) <- names(mode)
	return(pos.neg)
}
																	
	