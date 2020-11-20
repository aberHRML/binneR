#' Plot an infusion profile chromatogram
#' @rdname plotChromatogram
#' @description Plot an averaged infusion profile chromatogram from a Binalysis object. 
#' @param x S4 object of class \code{Binalysis}
#' @importFrom ggplot2 geom_vline geom_line labs ylab
#' @export

setMethod('plotChromatogram',signature = 'Binalysis',
					function(x){
						
						chromatograms <- x@spectra %>%
							.$headers
						scans <- x@binParameters@scans
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