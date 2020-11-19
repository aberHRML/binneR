#' @importFrom mzR openMSfile peaks
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom magrittr set_colnames set_names

getFile <- function(file,scans){
    ms <- openMSfile(file,backend = 'pwiz')
    
    hd <- header(ms) %>%
        select(seqNum,polarity,filterString) %>%
        group_by(polarity,filterString) %>%
        mutate(scan = 1:n()) %>%
        filter(scan %in% scans) 
        
    hd$polarity[hd$polarity == 0] <- 'n'
    hd$polarity[hd$polarity == 1] <- 'p'
    
    ms %>%
        peaks() %>%
        .[hd$seqNum] %>%
        map(~{
            d <- .
            d %>%
                set_colnames(c('mz','intensity')) %>%
                as_tibble()
        }) %>%
        set_names(hd$seqNum) %>%
        bind_rows(.id = 'seqNum') %>%
        mutate(seqNum = as.numeric(seqNum)) %>%
        left_join(hd, by = "seqNum") %>%
        select(-filterString,-seqNum)
}

#' @importFrom parallel makeCluster stopCluster parLapply
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
        mutate(mz = round(mz,5),bin = round(mz,2))
    return(pks)
}
