#' plotTIC
#' @rdname plotTIC
#' @description Plot sample total ion counts.
#' @param x S4 object of class Binalysis
#' @param by info column to plot against
#' @param colour info column to provide colour labels  
#' @importFrom stats IQR median
#' @importFrom dplyr bind_cols
#' @importFrom ggplot2 geom_point guide_legend guides geom_hline
#' @importFrom tibble rowid_to_column
#' @importFrom ggthemes scale_colour_ptol
#' @importFrom tidyr gather
#' @export

setMethod('plotTIC',signature = 'Binalysis',
					function(x, by = 'injOrder', colour = 'block'){
						rawInfo <- x %>%
							info()
						
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
						
						pl <- ggplot(TICdat,aes(x = Index,y = TIC,colour = Colour)) +
							geom_hline(data = TICmedian,aes(yintercept = Median)) +
							geom_hline(data = TICmedian,aes(yintercept = Q1),linetype = 2) +
							geom_hline(data = TICmedian,aes(yintercept = Q3),linetype = 2) +
							geom_hline(data = TICmedian,aes(yintercept = LowerOut),linetype = 3) +
							geom_hline(data = TICmedian,aes(yintercept = UpperOut),linetype = 3) +
							geom_point() +
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
							guides(colour = guide_legend(title = colour))
						
						if (length(unique(TICdat$Colour)) <= 12) {
							pl <- pl +
								scale_colour_ptol()
						}
						return(pl)
					}
					)
