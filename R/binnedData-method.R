#' binnedData-Binalysis
#' @rdname binnedData
#' @description Extract binned data from a Binalysis object.
#' @param x Binalysis object
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @export

setMethod('binnedData',signature = 'Binalysis',
          function(x){
              return(x@binnedData)
          })