#'
#' @export

massMat <- 
	function(x){ # to build intensity matrix for individal samples or all samples
	z <- data.frame(x[1])
	mat <- matrix(0,nrow=length(x),ncol=length(z[,1]))
	for (i in 1:length(x)){
		y <- data.frame(x[i])
		y <- t(y)
 		y <- y[2,]
		mat[i,] <- y
	}
	colnames(mat) <- z[,1]
	return (mat)
}