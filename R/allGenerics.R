
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

#' @rdname plotFingerprint
setGeneric('plotFingerprint',function(x){
	standardGeneric('plotFingerprint')
})

#' @rdname scans
setGeneric('scans',function(x){
	standardGeneric('scans')
})

#' @rdname modes
setGeneric('modes',function(x){
	standardGeneric('modes')
})

#' @rdname sranges
setGeneric('sranges',function(x){
	standardGeneric('sranges')
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