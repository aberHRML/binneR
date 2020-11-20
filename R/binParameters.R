#' Set spectral binning parameters
#' @description Selection of parameters to use for spectral binning.
#' @param scans numeric vector containing the scan indexes to use for binning
#' @param cls the column of class labels to use for aggregating accurate 
#' mass data. Defaults to NULL where accurate mass data will be averaged 
#' accross all samples
#' @param nCores the number of cores to use for parallel processing
#' @param clusterType the type of cluster to use for parallel processing
#' @examples 
#' p <- binParameters(scans = 6:17,nCores = 2,clusterType = detectClusterType())
#' 
#' 
#' @importFrom parallel detectCores
#' @export

binParameters <- function(scans = 5:12, 
                          cls = character(), 
                          nCores = detectCores() * 0.75, 
                          clusterType = 'FORK'){
   p <- new('BinParameters',
        scans = scans,
        cls = cls,
        nCores = nCores,
        clusterType = clusterType
        )
   
   if (.Platform$OS.type == 'windows') {
       p@clusterType <- 'PSOCK'    
   }
   
   return(p)
}


#' Get and set spectral binning parameters
#' @rdname parameters
#' @description Get and set spectral binning parameters.
#' @param x S4 object of class BinParameters
#' @param value parameter value to set
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

#' @rdname parameters
#' @export

setMethod('nCores',signature = 'BinParameters',function(x){
        x@nCores
})

#' @rdname parameters
#' @export

setMethod('nCores<-',signature = 'BinParameters',function(x,value){
        x@nCores <- value
        return(x)
})

#' @rdname parameters
#' @export

setMethod('clusterType',signature = 'BinParameters',function(x){
        x@clusterType
})

#' @rdname parameters
#' @export

setMethod('clusterType<-',signature = 'BinParameters',function(x,value){
        x@clusterType <- value
        return(x)
})





