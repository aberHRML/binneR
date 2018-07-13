#' @importFrom mzR openMSfile peaks
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble

getFile <- function(file,scans,sranges,modes){
    ms <- openMSfile(file,backend = 'pwiz')
    pks <- peaks(ms) %>%
        combScans(scans,sranges,modes) %>%
        purrr::map(bind_rows,.id = 'Scan') %>%
        bind_rows(.id = 'Mode') %>%
        tibble::as_tibble()
}

#' @importFrom parallel makeCluster stopCluster parLapply
#' @importFrom dplyr mutate

getPeaks <- function(files,scans,sranges,modes,nCores,clusterType){
   if (nCores < 1) {
       pks <- map(files,getFile,scans,sranges,modes)
   } else {
       clus <- makeCluster(nCores,type = clusterType)
       pks <- parLapply(clus,files,getFile,scans = scans,sranges = sranges,modes = modes)
       stopCluster(clus)
   }
    names(pks) <- files
    pks <- pks %>%
        bind_rows(.id = 'File') %>%
        mutate(mz = round(mz,5),Bin = round(mz,2))
    return(pks)
}
