# Collate all masses present in the supplied list 
# @name colMasses
# @description Collate all masses present across all scans in the supplied peak list, into a single reference vector.
# @param x A peak list containing peak matrices for each scan
# @return A vector containing all the masses present in the supplied peak list
# @author Jasen Finch

colMasses <- function(x){ 
 z <- NULL
  for (i in 1:length(x)) {
    y <- data.frame(x[[i]])
    y.1 <- as.numeric(y[,1])
    z <- c(z,y.1)  
  }
  z <- unique(z)
  z <- sort(z)
  return(z)
}