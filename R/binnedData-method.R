#' Extract spectrally binned data
#' @rdname binnedData
#' @description Extract binned data from a Binalysis object.
#' @param x Binalysis object
#' @return A list containing tibbles of bin feature intensities for each ionisation mode.
#' @seealso \code{\link{binneRlyse}} 
#' @export

setMethod('binnedData',signature = 'Binalysis',
          function(x){
              return(x@binnedData)
          })
