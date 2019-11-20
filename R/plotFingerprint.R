#' plotFingerprint
#' @rdname plotFingerprint
#' @description Plot averaged spectrum fingerprint.
#' @param x S4 object of class Binalysis
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