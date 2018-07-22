#' accurateData-Binalysis
#' @rdname accurateData
#' @description Extract accurate data from a Binalysis object.
#' @param x Binalysis object
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @export

setMethod('accurateData',signature = 'Binalysis',
          function(x){
              return(x@accurateMZ)
          })