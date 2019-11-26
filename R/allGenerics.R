
setGeneric("spectralBinning", function(x) {
    standardGeneric("spectralBinning")
})

setGeneric("ss", function(x) {
	standardGeneric("ss")
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

#' @rdname plotFingerprint
setGeneric('plotFingerprint',function(x){
	standardGeneric('plotFingerprint')
})

#' @rdname scans
setGeneric('scans',function(x){
	standardGeneric('scans')
})

#' @rdname cls
setGeneric('cls',function(x){
	standardGeneric('cls')
})

#' @rdname nCores
setGeneric('nCores',function(x){
	standardGeneric('nCores')
})

#' @rdname clusterType
setGeneric('clusterType',function(x){
	standardGeneric('clusterType')
})

#' @rdname plotPurity
setGeneric('plotPurity',function(x,histBins = 30){
	standardGeneric('plotPurity')
})

#' @rdname plotCentrality
setGeneric('plotCentrality',function(x,histBins = 30){
	standardGeneric('plotCentrality')
})