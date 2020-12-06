#' Plot a spectral bin feature
#' @rdname plotBin
#' @description Kernal density plot of a specified spectral bin feature.
#' @param x S4 object of class Binalysis
#' @param bin 0.01amu bin to plot
#' @param type bin to be plotted as a total (all), class (cls) or 
#' sample spectra.
#' @seealso \code{\link{accurateData}}, \code{\link{binneRlyse}}
#' @importFrom ggplot2 ggplot geom_density theme_bw xlim xlab ggtitle theme
#' @importFrom ggplot2 element_text facet_wrap aes
#' @importFrom stringr str_replace_all str_sub
#' @importFrom stats as.formula
#' @export

setMethod('plotBin',signature = 'Binalysis',
					function(x,bin,type = c('all','cls','sample')){
						
						type <- match.arg(type,c('all','cls','sample'))
						
						m <- bin %>%
							str_replace_all('[:alpha:]','') %>%
							as.numeric()
						
						mode <- bin %>%
							str_sub(1,1)
						
						dat <- x %>%
							spectra() %>%
							.$fingerprints %>%
							filter(polarity == mode & bin == m)
						
						if (nrow(dat) == 0) {
							stop('Bin not found.',call. = FALSE)
						}
						
						pl <- ggplot(dat,aes(x = mz)) +
							geom_density() +
							theme_bw() +
							xlim(m - 0.005,m + 0.005) +
							theme(plot.title = element_text(face = 'bold'),
										axis.title.y = element_text(face = 'bold'),
										axis.title.x = element_text(face = 'bold.italic'),
										axis.text.x = element_text(angle = 90,hjust = 1)) +
							labs(title = bin,
									 x = 'm/z',
									 y = 'Density')
						
						if (type == 'cls') {
							class <- cls(x)
							
							if (length(class) == 0) {
								stop('No "cls" parameter found for this Binalysis class object.',
										 call. = FALSE)
							}
								
							pl <- pl +
								facet_wrap(as.formula(paste("~", class)))
						}
						
						if (type == 'sample') {
							pl <- pl +
								facet_wrap(~fileName)
						}
						
						return(pl)
					}
)

#' Plot an infusion profile chromatogram
#' @rdname plotChromatogram
#' @description Plot an averaged infusion profile chromatogram from a 
#' Binalysis object. 
#' @param x S4 object of class \code{Binalysis}
#' @seealso \code{\link{binneRlyse}}
#' @importFrom ggplot2 geom_vline geom_line labs ylab
#' @export

setMethod('plotChromatogram',signature = 'Binalysis',
					function(x){
						
						chromatograms <- x %>%
							spectra() %>%
							.$headers
						scans <- x %>%
							scans()
						chromatograms <- chromatograms %>%
							dplyr::select(FileName,
														acquisitionNum,
														totIonCurrent,
														polarity,
														filterString) %>%
							split(.$polarity) %>%
							map(~{
								f <- .
								f %>% 
									split(.$FileName) %>%
									map(~{
										a <- .
										a %>%
											split(.$filterString) %>%
											map(~{
												b <- .
												b %>%
													mutate(acquisitionNum = seq_len(nrow(.)))
											}) %>%
											bind_rows()
									}) %>%
									bind_rows()
							}) %>% 
							bind_rows() %>%
							group_by(polarity,acquisitionNum) %>%
							summarise(totIonCurrent = mean(totIonCurrent))
						chromatograms$polarity[chromatograms$polarity == 0] <- 'Negative'
						chromatograms$polarity[chromatograms$polarity == 1] <- 'Positive'
						chromatograms %>%
							ggplot(aes(x = acquisitionNum,y = totIonCurrent)) +
							geom_line() +
							geom_vline(xintercept = min(scans),colour = 'red',linetype = 2) +
							geom_vline(xintercept = max(scans),colour = 'red',linetype = 2) +
							theme_bw() +
							labs(title = 'TIC chromatograms of infusion profile',
									 caption = 'Red lines indcate scan range used for spectral binning.') +
							theme(plot.title = element_text(face = 'bold'),
										axis.title = element_text(face = 'bold')) +
							facet_wrap(~polarity) +
							xlab('Scan') +
							ylab('Total Ion Current')
					}
)

#' Plot an infusion profile chromatogram from a file
#' @description Plot and averaged infusion profile from a vector of specified 
#' file paths.
#' @param files character vector of file paths to use
#' @param scans specify scans to highlight within the plot
#' @examples 
#' plotChromFromFile(metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1],
#'                             scans = c(6,18))
#' @export

plotChromFromFile <- function(files, scans = c()){
	
	chromatograms <- files %>%
		map(~{
			openMSfile(.,backend = 'pwiz') %>%
				header() %>%
				select(acquisitionNum,totIonCurrent,polarity,filterString) %>%
				split(.$polarity) %>%
				map(~{
					a <- .
					a %>%
						split(.$filterString) %>%
						map(~{
							b <- .
							b %>%
								mutate(acquisitionNum = seq_len(nrow(.)))
						}) %>%
						bind_rows()
				}) %>%
				bind_rows() %>%
				group_by(polarity,acquisitionNum) %>%
				summarise(totIonCurrent = mean(totIonCurrent))
		}) %>%
		bind_rows(.id = 'FileName') %>%
		group_by(polarity,acquisitionNum) %>%
		summarise(totIonCurrent = mean(totIonCurrent)) %>%
		as_tibble()
	chromatograms$polarity[chromatograms$polarity == 0] <- 'Negative'
	chromatograms$polarity[chromatograms$polarity == 1] <- 'Positive'
	pl <- chromatograms %>%
		{ggplot(.,aes(x = acquisitionNum,y = totIonCurrent)) +
				geom_line() +
				theme_bw() +
				labs(title = 'TIC chromatograms of infusion profile') +
				theme(plot.title = element_text(face = 'bold'),
							axis.title = element_text(face = 'bold')) +
				facet_wrap(~polarity) +
				xlab('Scan') +
				ylab('Total Ion Current')}
	if (length(scans) > 0) {
		pl <- pl +
			geom_vline(xintercept = min(scans),colour = 'red',linetype = 2) +
			geom_vline(xintercept = max(scans),colour = 'red',linetype = 2) 
	}
	return(pl)
}

#' Plot a fingerprint mass spectrum
#' @rdname plotFingerprint
#' @description Plot averaged spectrum fingerprint.
#' @param x S4 object of class Binalysis
#' @seealso \code{\link{binneRlyse}}
#' @importFrom ggplot2 geom_segment
#' @importFrom stringr str_remove_all
#' @importFrom dplyr summarise_all
#' @export

setMethod('plotFingerprint',signature = 'Binalysis',
					function(x){
						spectra <- x %>%
							binnedData() %>%
							map(~{
								tibble(Feature = colnames(.),
											 Intensity = colSums(.))
							}) %>%
							bind_rows() %>%
							mutate(Mode = str_sub(Feature,1,1),
										 `m/z` = str_remove_all(Feature,'[:alpha:]') %>%
										 	as.numeric())
						
						spectra$Mode[spectra$Mode == 'n'] <- 'Negative'
						spectra$Mode[spectra$Mode == 'p'] <- 'Positive'
						
						ggplot(spectra,aes(x = `m/z`,xend = `m/z`,y = 0,yend = Intensity)) +
							geom_segment() +
							theme_bw() +
							facet_wrap(~Mode,ncol = 1) +
							labs(title = 'Averaged spectrum fingerprint',
									 x = 'm/z',
									 y = 'Intensity') +
							theme(plot.title = element_text(face = 'bold'),
										axis.title = element_text(face = 'bold'))
					})

#' Plot bin purity histogram
#' @rdname plotPurity
#' @description Plot the bin purity distribution for a Binalysis object.
#' @param x S4 object of class Binalysis
#' @param histBins number of bins to use for histogram plotting
#' @seealso \code{\link{accurateData}}, \code{\link{binneRlyse}}, 
#' \code{\link{plotCentrality}}
#' @importFrom ggplot2 geom_histogram
#' @export

setMethod('plotPurity',signature = 'Binalysis',function(x,histBins = 30){
	
	pur <- x %>%
		accurateData() %>%
		select(polarity,bin,purity) %>%
		group_by(polarity,bin) %>%
		summarise(purity = mean(purity),.groups = 'drop')
	
	pur$polarity[pur$polarity == 'n'] <- 'Negative mode'
	pur$polarity[pur$polarity == 'p'] <- 'Positive mode'
	
	pur %>%
		ggplot(aes(x = purity)) +
		geom_histogram(fill = "#88CCEE",colour = 'black',bins = histBins) +
		theme_bw() +
		facet_wrap(~polarity) +
		ggtitle('Bin Purity Distribution') +
		theme(plot.title = element_text(face = 'bold'),
					axis.title = element_text(face = 'bold')) +
		xlab('Purity Measure') +
		ylab('Frequency')
	
})

#' Plot bin centrality histogram
#' @rdname plotCentrality
#' @description Plot the bin centrality distribution for a Binalysis object.
#' @param x S4 object of class Binalysis
#' @param histBins number of bins to use for histogram plotting
#' @seealso \code{\link{accurateData}}, \code{\link{binneRlyse}}, 
#' \code{\link{plotPurity}}
#' @export

setMethod('plotCentrality',signature = 'Binalysis',function(x,histBins = 30){
	
	pur <- x %>%
		accurateData() %>%
		select(polarity,bin,centrality) %>%
		group_by(polarity,bin) %>%
		summarise(centrality = mean(centrality),.groups = 'drop') 
	
	pur$polarity[pur$polarity == 'n'] <- 'Negative mode'
	pur$polarity[pur$polarity == 'p'] <- 'Positive mode'
	
	pur %>%
		ggplot(aes(x = centrality)) +
		geom_histogram(fill = "#88CCEE",colour = 'black',bins = histBins) +
		theme_bw() +
		facet_wrap(~polarity) +
		ggtitle('Bin Centrality Distribution') +
		theme(plot.title = element_text(face = 'bold'),
					axis.title = element_text(face = 'bold')) +
		xlab('Centrality measure') +
		ylab('Frequency')
	
})

#' Plot sample total ion counts
#' @rdname plotTIC
#' @description Plot sample total ion counts.
#' @param x S4 object of class Binalysis
#' @param by info column to plot against
#' @param colour info column to provide colour labels  
#' @seealso \code{\link{binneRlyse}}
#' @importFrom stats IQR median
#' @importFrom dplyr bind_cols
#' @importFrom ggplot2 geom_point guide_legend guides geom_hline
#' @importFrom tibble rowid_to_column
#' @importFrom ggthemes scale_fill_ptol
#' @importFrom tidyr gather
#' @export

setMethod('plotTIC',signature = 'Binalysis',
					function(x, by = 'injOrder', colour = 'block'){
						rawInfo <- x %>%
							sampleInfo()
						
						TICdat <- x %>%
							binnedData %>%
							map(rowSums) %>%
							bind_cols() %>%
							rowid_to_column(var = 'Sample') %>%
							mutate(Colour = rawInfo[,colour] %>% unlist() %>% factor(),
										 Index = rawInfo[,by] %>% unlist()) %>%
							gather('Mode','TIC',-Sample,-Colour,-Index)
						
						TICdat$Mode[TICdat$Mode == 'n'] <- 'Negative'
						TICdat$Mode[TICdat$Mode == 'p'] <- 'Positive'
						
						TICmedian <- TICdat %>%
							group_by(Mode) %>%
							summarise(Median = median(TIC),
												Q1 = Median - IQR(TIC),
												Q3 = Median + IQR(TIC),
												LowerOut = Q1 - IQR(TIC) * 1.5,
												UpperOut = Q3 + IQR(TIC) * 1.5)
						
						TICmedian[TICmedian < 0] <- 0
						
						pl <- ggplot(TICdat,aes(x = Index,y = TIC,fill = Colour)) +
							geom_hline(data = TICmedian,aes(yintercept = Median)) +
							geom_hline(data = TICmedian,aes(yintercept = Q1),linetype = 2) +
							geom_hline(data = TICmedian,aes(yintercept = Q3),linetype = 2) +
							geom_hline(data = TICmedian,aes(yintercept = LowerOut),linetype = 3) +
							geom_hline(data = TICmedian,aes(yintercept = UpperOut),linetype = 3) +
							geom_point(shape = 21) +
							theme_bw() +
							theme(plot.title = element_text(face = 'bold'),
										axis.title = element_text(face = 'bold'),
										legend.title = element_text(face = 'bold')) +
							facet_wrap(~Mode) +
							labs(title = 'Sample TICs',
									 caption = 'The solid line shows the median TIC across the sample set. 
The dashed line shows the inter-quartile range (IQR) and 
the dotted line shows the outlier boundary (1.5 X IQR).') +
							ylab('Total Ion Count') +
							xlab(by) +
							guides(fill = guide_legend(title = colour))
						
						if (length(unique(TICdat$Colour)) <= 12) {
							pl <- pl +
								scale_fill_ptol()
						}
						return(pl)
					}
)
