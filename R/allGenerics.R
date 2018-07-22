
setGeneric("spectralBinning", function(x) {
    standardGeneric("spectralBinning")
})

#' @rdname info
setGeneric("info", function(x) {
    standardGeneric("info")
})

#' @rdname binnedData
setGeneric("binnedData", function(x) {
    standardGeneric("binnedData")
})

#' @rdname accurateData
setGeneric("accurateData", function(x) {
    standardGeneric("accurateData")
})

#' @rdname plotBin
setGeneric('plotBin',function(x,bin,cls = T) {
	standardGeneric('plotBin')
})

#' @rdname plotChromatogram
setGeneric('plotChromatogram',function(x){
	standardGeneric('plotChromatogram')
})

#' @rdname plotTIC
setGeneric('plotTIC',function(x, by = 'injOrder', colour = 'block'){
	standardGeneric('plotTIC')
})