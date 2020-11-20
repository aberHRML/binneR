#' Extract sample information
#' @rdname info
#' @description Extract runinfo data from a Binalysis object.
#' @param x S4 object of class Binalysis
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @export

setMethod('info',signature = 'Binalysis',
          function(x){
              return(x@info)
          })