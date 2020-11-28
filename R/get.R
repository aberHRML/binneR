#' @importFrom mzR openMSfile peaks
#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom magrittr %>%

sampProcess <- function(file,scans,dp){
    
    pl <- getFile(file,scans) %>%
        mutate(mz = round(mz,dp)) %>% 
        group_by(polarity,mz) %>% 
        summarise(intensity = sum(intensity)/length(scans)) 
    
    return(pl)
}

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
        mutate(scan = 1:dplyr::n()) %>%
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

#' @importFrom parallel makeCluster stopCluster parLapply clusterExport
#' @importFrom dplyr mutate

getPeaks <- function(files,scans){
    
    pks <- future_map(files,getFile,scans = scans) %>%
        set_names(files) %>%
        bind_rows(.id = 'fileName') %>%
        mutate(fileName = basename(fileName),
               mz = round(mz,5),bin = round(mz,2))
    return(pks)
}

#' @importFrom mzR header

getHeaders <- function(files){
    
    headers <- files %>% 
        future_map(~{
        .x %>%
            openMSfile(backend = 'pwiz') %>%
            header()
    }) %>%
        set_names(files) %>%
        bind_rows(.id = 'FileName') %>%
        select(FileName,acquisitionNum,totIonCurrent,polarity,filterString)
    
    return(headers)
}

