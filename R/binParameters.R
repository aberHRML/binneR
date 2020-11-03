#' binParameters
#' @description selection of parameters to use for spectral binning.
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

#' scans
#' @rdname scans
#' @description get and set scans of a BinParameters object.
#' @param x an S4 object of class BinParameters
#' @export

setMethod('scans',signature = 'BinParameters',function(x){
        x@scans
})

#' @rdname scans
#' @param value numeric vector of scan numbers
#' @export

`scans<-` <- function(x,value){
        x@scans <- value
        return(x)
}

#' cls
#' @rdname cls
#' @description get and set class labels of a BinParameters object.
#' @param x an S4 object of class BinParameters
#' @export

setMethod('cls',signature = 'BinParameters',function(x){
        x@cls
})

#' @rdname cls
#' @param value character containing the column of class labels to use 
#' for aggregating accurate mass data
#' @export

`cls<-` <- function(x,value){
        x@cls <- value
        return(x)
}

#' nCores
#' @rdname nCores
#' @description get and set number of cores of a BinParameters object.
#' @param x an S4 object of class BinParameters
#' @export

setMethod('nCores',signature = 'BinParameters',function(x){
        x@nCores
})

#' @rdname nCores
#' @param value number of cores to use for parallel processing
#' @export

`nCores<-` <- function(x,value){
        x@nCores <- value
        return(x)
}

#' clusterType
#' @rdname clusterType
#' @description get and set cluster type of a BinParameters object.
#' @param x an S4 object of class BinParameters
#' @export

setMethod('clusterType',signature = 'BinParameters',function(x){
        x@clusterType
})

#' @rdname clusterType
#' @param value cluster type to use for parallel processing. FORK or PSOCK.
#' @export

`clusterType<-` <- function(x,value){
        x@clusterType <- value
        return(x)
}





