process.ind <- 
	function(file,scans.1=scans,res.1=res,cut.1=cut,srce.1=srce){
  source(srce.1)
  print(file)
  library(mzR)
  flush.console()
  aa <- openMSfile(file[[1]])
  pl <- peaks(aa) # obtain peak lists
  seq.p <- seq(2,length(pl),2)
  seq.n <- seq(1,length(pl),2)
  pos.s <- pl[seq.p]  # separate positive and negative scans 
  neg.s <- pl[seq.n]
  pl.p <- pos.s[scans.1]  # range of scans to use 
  pl.n <- neg.s[scans.1]
  print("pos")
  flush.console()
  pl.p.1 <- samp.process(pl.p,res.1,cut.1)
  print("neg")
  flush.console()
  pl.n.1 <- samp.process(pl.n,res.1,cut.1)
  pos <- pl.p.1
  neg <- pl.n.1
  pos.neg <- list(pos,neg)
  return(pos.neg)
}