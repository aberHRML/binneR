#' info-Binalysis
#' @rdname info
#' @description Extract runinfo data from a Binalysis object.
#' @param x Binalysis object
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @export

setMethod('info',signature = 'Binalysis',
          function(x){
              return(x@info)
          })