#' Extract sample information
#' @rdname info
#' @description Extract sample meta information from a Binalysis object.
#' @param x S4 object of class Binalysis
#' @return A tibble containing sample meta information.
#' @seealso \code{\link{Binalysis-class}}, \code{\link{binneRlyse}}
#' @export

setMethod('info',signature = 'Binalysis',
					function(x){
						return(x@info)
					})

#' Extract spectrally binned data
#' @rdname binnedData
#' @description Extract binned data from a Binalysis object.
#' @param x Binalysis object
#' @return A list containing tibbles of bin feature intensities for each 
#' ionisation mode.
#' @seealso \code{\link{binneRlyse}} 
#' @export

setMethod('binnedData',signature = 'Binalysis',
					function(x){
						return(x@binnedData)
					})

#' Extract accurate mass data
#' @rdname accurateData
#' @description Extract sample-wise accurate mass data and bin measures 
#' for each 0.01 amu bin from a Binalysis object.
#' @param x S4 object of class Binalysis
#' @return A tibble containing sample wise accurate mass data.
#' @seealso \code{\link{binneRlyse}}
#' @export

setMethod('accurateData',signature = 'Binalysis',
					function(x){
						return(x@accurateMZ)
					})
