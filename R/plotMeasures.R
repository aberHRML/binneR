#' plotPurity
#' @rdname plotPurity
#' @description Plot the bin purity distribution for a Binalysis object.
#' @param x S4 object of class Binalysis
#' @param histBins number of bins to use for histogram plotting
#' @importFrom ggplot2 geom_histogram
#' @export

setMethod('plotPurity',signature = 'Binalysis',function(x,histBins = 30){
	
	pur <- x %>%
		accurateData() %>%
		dplyr::select(polarity,Purity) %>%
		mutate(polarity = as.character(polarity)) 
	
	pur$polarity[pur$polarity == 'n'] <- 'Negative mode'
	pur$polarity[pur$polarity == 'p'] <- 'Positive mode'
	
	pur %>%
		ggplot(aes(x = Purity)) +
		geom_histogram(fill = "#88CCEE",colour = 'black',bins = histBins) +
		theme_bw() +
		facet_wrap(~polarity) +
		ggtitle('Bin Purity Distribution') +
		theme(plot.title = element_text(face = 'bold'),
					axis.title = element_text(face = 'bold')) +
		xlab('Purity Measure') +
		ylab('Frequency')

})

#' plotCentrality
#' @rdname plotCentrality
#' @description Plot the bin centrality distribution for a Binalysis object.
#' @param x S4 object of class Binalysis
#' @param histBins number of bins to use for histogram plotting
#' @export

setMethod('plotCentrality',signature = 'Binalysis',function(x,histBins = 30){
	
	pur <- x %>%
		accurateData() %>%
		dplyr::select(polarity,Centrality) %>%
		mutate(polarity = as.character(polarity)) 
	
	pur$polarity[pur$polarity == 'n'] <- 'Negative mode'
	pur$polarity[pur$polarity == 'p'] <- 'Positive mode'
	
	pur %>%
		ggplot(aes(x = Centrality)) +
		geom_histogram(fill = "#88CCEE",colour = 'black',bins = histBins) +
		theme_bw() +
		facet_wrap(~polarity) +
		ggtitle('Bin Centrality Distribution') +
		theme(plot.title = element_text(face = 'bold'),
					axis.title = element_text(face = 'bold')) +
		xlab('Centrality measure') +
		ylab('Frequency')
	
})