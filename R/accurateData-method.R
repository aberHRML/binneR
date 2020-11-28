#' Extract accurate mass data
#' @rdname accurateData
#' @description Extract sample-wise accurate mass data and bin measures for each 0.01 amu bin from a Binalysis object.
#' @param x S4 object of class Binalysis
#' @return A tibble containing sample wise accurate mass data.
#' @seealso \code{\link{binneRlyse}}
#' @export

setMethod('accurateData',signature = 'Binalysis',
          function(x){
              return(x@accurateMZ)
          })