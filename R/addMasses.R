#' add in masses with zero intensity if not present
#' @export

addMasses <- 
	function(x){
  masses <- colMasses(x)
  y.1 <- NULL
  for (i in 1:length(x)){
    y <- data.frame(x[[i]])
    mat <- matrix(0,nrow=length(masses),ncol=2)
    names(mat) <- c("mz","intensity")
    z <- masses %in% y[,1]
    mat[,1] <- masses
		mat[,2] <- replace(mat[,2],z,y[,2])
    y.1[i] <- list(mat)
  }
  return(y.1)
}
