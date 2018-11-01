#' plotBin
#' @rdname plotBin
#' @description kernal density plot of a specified bin.
#' @param x S4 object of class Binalysis
#' @param bin 0.01amu bin to plot
#' @param cls \code{TRUE} or \code{FALSE}. Should bins be plotted by class?
#' @importFrom ggplot2 ggplot geom_density theme_bw xlim xlab ggtitle theme
#' @importFrom ggplot2 element_text facet_wrap aes
#' @importFrom stringr str_replace_all str_sub
#' @export

setMethod('plotBin',signature = 'Binalysis',
					function(x,bin,cls = T){
						
						m <- bin %>%
							str_replace_all('[:alpha:]','') %>%
							as.numeric()
						mode <- bin %>%
							str_sub(1,1)
						
						dat <- x %>%
							.@spectra %>%
							.$fingerprints %>%
							filter(Mode == mode & Bin == m)
						
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
						
						if (cls == T) {
							pl <- pl +
								facet_wrap(~Class)
						}
						return(pl)
					}
)
