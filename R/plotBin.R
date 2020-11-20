#' plotBin
#' @rdname plotBin
#' @description kernal density plot of a specified bin.
#' @param x S4 object of class Binalysis
#' @param bin 0.01amu bin to plot
#' @param type bin to be plotted as a total (all), class (cls) or sample spectra.
#' @importFrom ggplot2 ggplot geom_density theme_bw xlim xlab ggtitle theme
#' @importFrom ggplot2 element_text facet_wrap aes
#' @importFrom stringr str_replace_all str_sub
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
							.@spectra %>%
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
							class <- cls(x@binParameters)
							
							if (length(class) == 0) {
								stop('No "cls" parameter found for this Binalysis class object.',call. = FALSE)
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
