#' Set spectral binning parameters
#' @description Selection of parameters to use for spectral binning.
#' @param scans numeric vector containing the scan indexes to use for binning
#' @param cls the column of class labels to use for aggregating accurate 
#' mass data. Defaults to NULL where accurate mass data will be averaged 
#' accross all samples
#' @return S4 object of class BinParameters
#' @examples 
#' p <- binParameters(scans = 6:17)
#' @seealso \code{\link{BinParameters-class}}, \code{\link{scans}}, 
#' \code{\link{cls}}
#' @importFrom parallel detectCores
#' @export

binParameters <- function(scans = 5:12, 
                          cls = character()){
   p <- new('BinParameters',
        scans = scans,
        cls = cls
        )
   
   return(p)
}


#' Get and set spectral binning parameters
#' @rdname parameters
#' @description Get and set spectral binning parameters.
#' @param x S4 object of class BinParameters
#' @param value parameter value to set
#' @seealso \code{\link{BinParameters-class}}, \code{\link{binParameters}}
#' @export

setMethod('scans',signature = 'BinParameters',function(x){
        x@scans
})

#' @rdname parameters
#' @export

setMethod('scans<-',signature = 'BinParameters',
          function(x,value){
        x@scans <- value
        return(x)
})

#' @rdname parameters
#' @export

setMethod('cls',signature = 'BinParameters',function(x){
        x@cls
})

#' @rdname parameters
#' @export

setMethod('cls<-',signature = 'BinParameters',function(x,value){
        x@cls <- value
        return(x)
})
