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
