#' Show methods for spectral binning classes
#' @rdname show
#' @description Show methods for spectral binning classes.
#' @param object S4 objects of class BinParameters or Binanalysi
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @importFrom methods show
#' @importFrom purrr map_chr
#' @export

setMethod('show',signature = 'BinParameters',
					function(object){
						cat('\n')
						cat('Scans:',
								paste(min(scans(object)),':',
											max(scans(object)),sep = ''),
								'\n')
						if (length(cls(object)) > 0) {
							cat('Class:',cls(object),'\n')
						}
					})

#' @rdname show
#' @export

setMethod('show',signature = 'Binalysis',
					function(object){
						
						cat('\n')
						cat(str_c(blue('binneR'),red(str_c('v',version(object))),sep = ' '))
						cat('\n')  
						cat(creationDate(object))
						cat('\n')  
						
						cat('Samples:',length(filePaths(object)))
						cat('\n')
						
						if (length(binnedData(object)) > 0) {
							var <- lapply(binnedData(object),ncol)
							var <- map_chr(names(var),~{
								str_c(.,': ',var[[.]],' features')
							}) %>%
								str_c(collapse = '\n')
							cat(var,sep = '\n')
						}
						if (nrow(accurateData(object)) > 0) {
							cat('Average Purity:',
									mean(accurateData(object)$purity,
											 na.rm = TRUE) %>% 
										round(3),
									'\n')
							cat('Average Centrality:',
									mean(accurateData(object)$centrality,
											 na.rm = TRUE) %>% 
										round(3),
									'\n')	
						}
						cat('\n')
					})
