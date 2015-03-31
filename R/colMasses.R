#' Collate all masses present in all lists 

colMasses <- 
	function(x){ 
  z <- NULL
  for (i in 1:length(x)){
    y <- data.frame(x[[i]])
    y.1 <- as.numeric(y[,1])
    z <- c(z,y.1)  
  }
  z <- unique(z)
  z <- sort(z)
  return(z)
}