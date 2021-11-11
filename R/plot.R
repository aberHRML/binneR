#' @importFrom ggplot2 theme_bw element_blank element_line element_text

plotTheme <- function(){
	theme_bw() +
		theme(plot.title = element_text(face = 'bold',hjust = 0.5),
								plot.caption = element_text(hjust = 0),
								panel.border = element_blank(),
								panel.grid = element_blank(),
								axis.title = element_text(face = 'bold'),
								axis.line = element_line(),
								legend.title = element_text(face = 'bold'),
								strip.background = element_blank(),
								strip.text = element_text(face = 'bold'))
}

binPlot <- function(dat,bin,m,dp,type,cls){
	pl <- ggplot(dat,aes(x = mz)) +
		geom_density() +
		xlim(m - 5 * 10^-(dp + 1),
							m + 5 * 10^-(dp + 1)) +
		plotTheme() +
		scale_y_continuous(expand = c(0,0)) +
		labs(title = bin,
							x = 'm/z',
							y = 'Density')
	
	if (type == 'cls') {
		
		if (length(cls) == 0) {
			stop('No "cls" parameter found for this Binalysis class object.',
								call. = FALSE)
		}
		
		pl <- pl +
			facet_wrap(as.formula(paste("~", cls)))
	}
	
	if (type == 'sample') {
		pl <- pl +
			facet_wrap(~fileName)
	}
	
	return(pl)
}

#' Plot a spectral bin feature
#' @rdname plotBin
#' @description Kernal density plot of a specified spectral bin feature.
#' @param x S4 object of class Binalysis
#' @param bin 0.01amu bin to plot
#' @param type bin to be plotted as a total (all), class (cls) or 
#' sample spectra.
#' @seealso \code{\link{accurateData}}, \code{\link{binneRlyse}}
#' @importFrom ggplot2 ggplot geom_density xlim xlab ggtitle theme
#' @importFrom ggplot2 facet_wrap aes
#' @importFrom stringr str_replace_all str_sub str_extract
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
						
						dp <- str_extract(bin,'(?<=[.])[\\w+.-]+') %>% 
							nchar()
						
						class <- cls(x)
						
						binPlot(dat,bin,m,dp,type,class)
					}
)

plotChrom <- function(chromatograms,scans){
	pl <- ggplot(chromatograms,
								aes(x = acquisitionNum,y = totIonCurrent)) +
		geom_line() +
		plotTheme() +
		scale_y_continuous(expand = c(0,0)) +
		labs(title = 'TIC chromatograms of infusion profile') +
		facet_wrap(~polarity,
													scales = 'free',
													ncol = 1) +
		xlab('Scan') +
		ylab('Total Ion Current')
	
	if (length(scans) > 0) {
		pl <- pl +
			labs(caption = 'Red lines indcate scan range used for spectral binning.') +
			geom_vline(xintercept = min(scans),colour = 'red',linetype = 2) +
			geom_vline(xintercept = max(scans),colour = 'red',linetype = 2) 
	}
	
	return(pl)
}

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
						
						chromatograms$polarity[chromatograms$polarity == 0] <- 'Negative mode'
						chromatograms$polarity[chromatograms$polarity == 1] <- 'Positive mode'
						
						chromatograms %>%
							plotChrom(scans)
					}
)

#' Plot an infusion profile chromatogram from a file
#' @description Plot and averaged infusion profile from a vector of specified 
#' file paths.
#' @param files character vector of file paths to use
#' @param scans specify scans to highlight within the plot
#' @examples 
#' file_paths <- system.file('example-data/1.mzML.gz',package = 'binneR')
#' 
#' plotChromFromFile(file_paths, 
#'                   scans = detectInfusionScans(file_paths))
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
	
	chromatograms$polarity[chromatograms$polarity == 0] <- 'Negative mode'
	chromatograms$polarity[chromatograms$polarity == 1] <- 'Positive mode'
	
	chromatograms %>%
		plotChrom(scans)
	
}

#' @importFrom ggplot2 geom_segment

plotSpectrum <- function(spectra){
	ggplot(spectra,aes(x = `m/z`,xend = `m/z`,y = 0,yend = Intensity)) +
		geom_segment() +
		plotTheme() +
		scale_y_continuous(expand = c(0,0)) +
		facet_wrap(~Mode,ncol = 1,scales = 'free') +
		labs(title = 'Averaged spectrum fingerprint',
							x = 'm/z',
							y = 'Intensity')
}

#' Plot a fingerprint mass spectrum
#' @rdname plotFingerprint
#' @description Plot averaged spectrum fingerprint.
#' @param x S4 object of class Binalysis
#' @seealso \code{\link{binneRlyse}}
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
						
						spectra$Mode[spectra$Mode == 'n'] <- 'Negative mode'
						spectra$Mode[spectra$Mode == 'p'] <- 'Positive mode'
						
						plotSpectrum(spectra)
					})

#' @importFrom ggplot2 aes_string geom_histogram scale_y_continuous

plotHist <- function(d,x,histBins,title,xlab,ylab){
	ggplot(d,aes_string(x = x)) +
		geom_histogram(fill = ggthemes::ptol_pal()(1),colour = 'black',bins = histBins) +
		plotTheme() +
		facet_wrap(~polarity) +
		scale_y_continuous(expand = c(0,0)) +
		labs(title = title,
							x = xlab,
							y = ylab)
}

#' Plot bin purity histogram
#' @rdname plotPurity
#' @description Plot the bin purity distribution for a Binalysis object.
#' @param x S4 object of class Binalysis
#' @param histBins number of bins to use for histogram plotting
#' @seealso \code{\link{accurateData}}, \code{\link{binneRlyse}}, 
#' \code{\link{plotCentrality}}
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
		plotHist('purity',
											histBins = histBins,
											title = 'Bin Purity Distribution',
											xlab = 'Purity',
											ylab = 'Frequency')
	
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
		plotHist('centrality',
											histBins = histBins,
											title = 'Bin Centrality Distribution',
											xlab = 'Centrality',
											ylab = 'Frequency')
	
})

#' @importFrom ggplot2 geom_point guide_legend guides geom_hline

TICplot <- function(TICdat,TICmedian,by,colour){
	pl <- ggplot(TICdat,aes(x = Index,y = TIC,fill = Colour)) +
		geom_hline(data = TICmedian,aes(yintercept = Median)) +
		geom_hline(data = TICmedian,aes(yintercept = Q1),linetype = 2) +
		geom_hline(data = TICmedian,aes(yintercept = Q3),linetype = 2) +
		geom_hline(data = TICmedian,aes(yintercept = LowerOut),linetype = 3) +
		geom_hline(data = TICmedian,aes(yintercept = UpperOut),linetype = 3) +
		geom_point(shape = 21) +
		plotTheme() +
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

#' Plot sample total ion counts
#' @rdname plotTIC
#' @description Plot sample total ion counts.
#' @param x S4 object of class Binalysis
#' @param by info column to plot against
#' @param colour info column to provide colour labels  
#' @seealso \code{\link{binneRlyse}}
#' @importFrom stats IQR median
#' @importFrom dplyr bind_cols
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
						
						TICdat$Mode[TICdat$Mode == 'n'] <- 'Negative mode'
						TICdat$Mode[TICdat$Mode == 'p'] <- 'Positive mode'
						
						TICmedian <- TICdat %>%
							group_by(Mode) %>%
							summarise(Median = median(TIC),
												Q1 = Median - IQR(TIC),
												Q3 = Median + IQR(TIC),
												LowerOut = Q1 - IQR(TIC) * 1.5,
												UpperOut = Q3 + IQR(TIC) * 1.5)
						
						TICmedian[TICmedian < 0] <- 0
						
						pl <- TICplot(TICdat,TICmedian,by,colour)
						return(pl)
					}
)
