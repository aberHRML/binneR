
setGeneric("spectralBinning", function(x)
	standardGeneric("spectralBinning"))

setGeneric("ss", function(x)
	standardGeneric("ss"))

#' @rdname results
setGeneric("version", function(x)
	standardGeneric("version"))

#' @rdname results
setGeneric("creationDate", function(x)
	standardGeneric("creationDate"))

#' @rdname results
setGeneric("filePaths", function(x)
	standardGeneric("filePaths"))

#' @rdname results
setGeneric("sampleInfo", function(x)
	standardGeneric("sampleInfo"))

#' @rdname results
setGeneric("binnedData", function(x)
	standardGeneric("binnedData"))

#' @rdname results
setGeneric("accurateData", function(x)
	standardGeneric("accurateData"))

#' @rdname plotBin
setGeneric('plotBin',
					 function(x,bin,
					 				 type = c('all','cls','sample'))
					 	standardGeneric('plotBin'))

#' @rdname plotChromatogram
setGeneric('plotChromatogram',function(x)
	standardGeneric('plotChromatogram'))

#' @rdname plotTIC
setGeneric('plotTIC',
					 function(x, by = 'injOrder', colour = 'block')
	standardGeneric('plotTIC'))

#' @rdname plotFingerprint
setGeneric('plotFingerprint',function(x)
	standardGeneric('plotFingerprint'))

#' @rdname parameters
setGeneric('scans',function(x)
	standardGeneric('scans'))

#' @rdname parameters
setGeneric('scans<-',function(x,value)
	standardGeneric('scans<-'))

#' @rdname parameters
setGeneric('cls',function(x)
	standardGeneric('cls'))

#' @rdname parameters
setGeneric('cls<-',function(x,value)
	standardGeneric('cls<-'))

#' @rdname plotPurity
setGeneric('plotPurity',function(x,histBins = 30)
	standardGeneric('plotPurity'))

#' @rdname plotCentrality
setGeneric('plotCentrality',function(x,histBins = 30)
	standardGeneric('plotCentrality'))
