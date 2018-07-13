#' show-BinParameters
#' @description show method for BinParameters class
#' @param object BinParameters Object
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @importFrom methods show
#' @export

setMethod('show',signature = 'BinParameters',
          function(object){
              cat('\n')
              cat('Scans:',paste(min(object@scans),':',max(object@scans),sep = ''),'\n')
              cat('Modes:',paste(object@modes,collapse = ', '),'\n')
              cat('Scan Ranges:',paste(unlist(lapply(object@sranges,function(x){
                 paste(x[1],':',x[2],sep = '') 
              })),collapse = ' '),'\n')
              if (length(object@cls) > 0) {
                  cat('Class:',object@cls,'\n')
              }
              cat('No. Cores',object@nCores,'\n')
              cat('Cluster Type:',object@clusterType,'\n')
          })

#' show-Binalysis
#' @description show method for Binalysis class
#' @param object Binalysis Object
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @importFrom methods show
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
                  names(var) <- names(object@binnedData)
                  var <- bind_rows(var)
                  var <- apply(var,1,paste,collapse = ': ')
                  var <- paste(var,'features')
                  cat(var,sep = '\n')
              }
              cat('Average Purity:',mean(object@accurateMZ$Purity,na.rm = T),'\n')
              cat('Average Centrality:',mean(object@accurateMZ$Centrality,na.rm = T),'\n')
              cat('\n')
          })