#' @importFrom mzR openMSfile peaks
#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom magrittr %>%

sampProcess <- function(file,scans,dp){
    
    `%>%` <- getFromNamespace('%>%','magrittr')
    
    pl <- binneR::getFile(file,scans) %>%
        dplyr::mutate(mz = round(mz,dp)) %>% 
        dplyr::group_by(polarity,mz) %>% 
        dplyr::summarise(intensity = sum(intensity)/length(scans)) 
    
    return(pl)
}

#' @importFrom mzR openMSfile peaks
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom magrittr set_colnames set_names

getFile <- function(file,scans){
    
    `%>%` <- getFromNamespace('%>%','magrittr')
    
    ms <- mzR::openMSfile(file,backend = 'pwiz')
    
    hd <- mzR::header(ms) %>%
        dplyr::select(seqNum,polarity,filterString) %>%
        dplyr::group_by(polarity,filterString) %>%
        dplyr::mutate(scan = 1:dplyr::n()) %>%
        dplyr::filter(scan %in% scans) 
        
    hd$polarity[hd$polarity == 0] <- 'n'
    hd$polarity[hd$polarity == 1] <- 'p'
    
    ms %>%
        mzR::peaks() %>%
        .[hd$seqNum] %>%
        purrr::map(~{
            d <- .
            d %>%
                magrittr::set_colnames(c('mz','intensity')) %>%
                tibble::as_tibble()
        }) %>%
        magrittr::set_names(hd$seqNum) %>%
        dplyr::bind_rows(.id = 'seqNum') %>%
        dplyr::mutate(seqNum = as.numeric(seqNum)) %>%
        dplyr::left_join(hd, by = "seqNum") %>%
        dplyr::select(-filterString,-seqNum)
}

#' @importFrom parallel makeCluster stopCluster parLapply clusterExport
#' @importFrom dplyr mutate

getPeaks <- function(files,scans,nCores,clusterType){
   if (nCores < 1) {
       pks <- map(files,getFile,scans)
   } else {
       clus <- makeCluster(nCores,type = clusterType)
       
       pks <- parLapply(clus,files,getFile,scans = scans)
       
       stopCluster(clus)
   }
    names(pks) <- files
    pks <- pks %>%
        bind_rows(.id = 'fileName') %>%
        mutate(fileName = basename(fileName),
               mz = round(mz,5),bin = round(mz,2))
    return(pks)
}
