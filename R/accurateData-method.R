#' Extract accurate mass data
#' @rdname accurateData
#' @description Extract accurate data mass from a Binalysis object.
#' @param x S4 object of class Binalysis
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @export

setMethod('accurateData',signature = 'Binalysis',
          function(x){
              return(x@accurateMZ)
          })