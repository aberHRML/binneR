#' Add in masses with zero intensity if not present in a peak matrix
#' @name addMasses
#' @description This function will add rows to a peak matrix of missing m/z identified within the supplied list to ensure equal matrix row lengths for further processing  
#' @param x A list object containing peak matrices for each scan
#' @return A list object containing peak matrices with added 0 values
#' @author Jasen Finch
#' @export

addMasses <- function(x){
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
