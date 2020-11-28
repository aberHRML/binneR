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
              		paste(min(object@scans),':',
              					max(object@scans),sep = ''),
              		'\n')
              if (length(object@cls) > 0) {
                  cat('Class:',object@cls,'\n')
              }
          })

#' @rdname show
#' @export

setMethod('show',signature = 'Binalysis',
          function(object){
              cat('\n')
              if (length(object@binLog) > 0) {
                  cat(object@binLog)
                  cat('\n')  
              }
              cat('Samples:',length(object@files))
              cat('\n')
              if (length(object@binLog) > 0) {
                  var <- lapply(object@binnedData,ncol)
                  var <- map_chr(names(var),~{
                  	str_c(.,': ',var[[.]],' features')
                  }) %>%
                  	str_c(collapse = '\n')
                  cat(var,sep = '\n')
              }
              if (length(object@accurateMZ) > 0) {
              	cat('Average Purity:',
              			mean(object@accurateMZ$purity,
              					 na.rm = TRUE) %>% 
              				round(3),
              			'\n')
              	cat('Average Centrality:',
              			mean(object@accurateMZ$centrality,
              					 na.rm = TRUE) %>% 
              				round(3),
              			'\n')	
              }
              cat('\n')
          })