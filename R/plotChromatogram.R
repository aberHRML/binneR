#' plotChromatogram
#' @rdname plotChromatogram
#' @description plot an averaged infusion profile from a Binalysis object. 
#' @param x S4 object of class \code{Binalysis}
#' @importFrom ggplot2 geom_vline geom_line labs ylab
#' @export

setMethod('plotChromatogram',signature = 'Binalysis',
					function(x){
						
						chromatograms <- x@spectra %>%
							.$headers
						scans <- x@binParameters@scans
						chromatograms <- chromatograms %>%
							dplyr::select(FileName,acquisitionNum,totIonCurrent,polarity) %>%
							split(.$polarity) %>%
							map(~{
								f <- .
								f %>% 
									split(.$FileName) %>%
									map(~{
										a <- .
										a %>%
											split(rep(1:length(x@binParameters@sranges),nrow(.)/length(x@binParameters@sranges))) %>%
											map(~{
												b <- .
												b %>%
													mutate(acquisitionNum = 1:nrow(.))
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

#' plotChromFromFile
#' @description plot and averaged infusion profile from a vector of specified file paths.
#' @param files character vector of file paths to use
#' @param sranges A list of vectors containing the scan events present
#' @param scans specify scans to highlight within the plot
#' @examples 
#' plotChromFromFile(list.files(system.file('mzML',package = 'binneR'),
#'                             full.names=TRUE),scans = c(6,18))
#' @export

plotChromFromFile <- function(files, sranges = list(c(70,1000)), scans = c()){
	
	chromatograms <- files %>%
		map(~{
			openMSfile(.) %>%
				header() %>%
				select(acquisitionNum,totIonCurrent,polarity) %>%
				split(.$polarity) %>%
				map(~{
					a <- .
					a %>%
						split(rep(1:length(sranges),nrow(.)/length(sranges))) %>%
						map(~{
							b <- .
							b %>%
								mutate(acquisitionNum = 1:nrow(.))
						}) %>%
						bind_rows()
				}) %>%
				bind_rows() %>%
				group_by(polarity,acquisitionNum) %>%
				summarise(totIonCurrent = mean(totIonCurrent))
		}) %>%
		bind_rows(.id = 'FileName') %>%
		group_by(polarity,acquisitionNum) %>%
		summarise(totIonCurrent = mean(totIonCurrent))
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