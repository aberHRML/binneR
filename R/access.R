#' Binalysis class get methods
#' @rdname results
#' @description Methods for accessing spectral binning results from the  Binalysis S4 class.
#' @param x S4 object of class Binalysis
#' @param value value to set
#' @details 
#' \itemize{
#'  \item{sampleInfo}{ - Extract sample meta information.}
#'  \item{binnedData}{ - Extract a binned data intensity matrix.}
#'  \item{accurateData}{ - Extract sample-wise accurate mass data and bin measures.}
#' }
#' @seealso \code{\link{Binalysis-class}}, \code{\link{binneRlyse}}
#' @export

setMethod('version',signature = 'Binalysis',
					function(x){
						x@version
					})

#' @rdname results
#' @export

setMethod('creationDate',signature = 'Binalysis',
					function(x){
						x@creation_date
					})

#' @rdname results
#' @export

setMethod('filePaths',signature = 'Binalysis',
					function(x){
						x@file_paths
					})

#' @rdname results
#' @export

setMethod('sampleInfo',signature = 'Binalysis',
					function(x){
						x@sample_info
					})

#' @rdname results
#' @export

setMethod('binnedData',signature = 'Binalysis',
					function(x){
						return(x@binnedData)
					})

#' @rdname results
#' @export

setMethod('accurateData',signature = 'Binalysis',
					function(x){
						return(x@accurateMZ)
					})

# setMethod('binningParameters',signature = 'Binalysis',
# 					function(x){
# 						
# 					})