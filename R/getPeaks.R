#' @importFrom mzR openMSfile peaks
#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom magrittr %>%
#' @importFrom utils getFromNamespace

sampProcess <- function(file,scans,dp){
    
    `%>%` <- getFromNamespace('%>%','magrittr')
    getFile <- getFromNamespace('getFile','binneR')
    
    pl <- getFile(file,scans)
    pl$mz <- round(pl$mz,dp)
    pl <- pl %>%
        split(stringr::str_c(.$polarity,.$mz)) %>%
        purrr::map(~{
            .x$intensity = sum(.x$intensity)/length(scans)
            return(.x)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::select(polarity,mz,intensity) %>%
        dplyr::distinct()
    
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
        split(stringr::str_c(.$polarity,.$filterString)) %>%
        purrr::map(~{
            .x$scan <- seq_len(nrow(.x))
            return(.x)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::filter(scan %in% scans)
    
    hd$polarity[hd$polarity == 0] <- 'n'
    hd$polarity[hd$polarity == 1] <- 'p'
    
    ms <- ms %>%
        mzR::peaks() %>%
        .[hd$seqNum] %>%
        purrr::map(~{
            d <- .
            d %>%
                magrittr::set_colnames(c('mz','intensity')) %>%
                tibble::as_tibble()
        }) %>%
        magrittr::set_names(hd$seqNum) %>%
        dplyr::bind_rows(.id = 'seqNum')
    
    ms$seqNum <- as.numeric(ms$seqNum)
    
    ms %>%
        dplyr::left_join(hd, by = "seqNum") %>%
        dplyr::select(-filterString,-seqNum)
}

#' @importFrom parallel makeCluster stopCluster parLapply
#' @importFrom dplyr mutate

getPeaks <- function(files,scans,nCores,clusterType){
    if (nCores < 1) {
        pks <- map(files,getFile,scans)
    } else {
        
        clus <- makeCluster(nCores,type = clusterType)
        
        pks <- parLapply(clus,
                         files,
                         getFile,
                         scans = scans)
        
        stopCluster(clus)
    }
    names(pks) <- files
    pks <- pks %>%
        bind_rows(.id = 'fileName') %>%
        mutate(fileName = basename(fileName),
               mz = round(mz,5),bin = round(mz,2))
    return(pks)
}
