#' build an intensity matrix for supplied peak list
#' @name massMat
#' @description build and intensity matrix for a supplied peak list. Can be used to build intensity matrices for combining either scans within a sample of multiple samples. 
#' @param x A peak list containing peak matrices for each scan of equal row length
#' @return An intensity matrix with as many rows as peak matrices in peak list
#' @author Jasen Finch
#' @export

massMat <- function(x){ 
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