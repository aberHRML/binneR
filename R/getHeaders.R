#' @importFrom mzR header

getHeaders <- function(files,nCores,clusterType){
    if (nCores < 1) {
        headers <- map(files,~{
            f <- .
            f %>%
                openMSfile(backend = 'pwiz') %>%
                header()
        })
    } else {
        clus <- makeCluster(nCores,type = clusterType)
        headers <- parLapply(clus,files,function(x){
            x %>%
                openMSfile(backend = 'pwiz') %>%
                mzR::header()
        })
        stopCluster(clus)
    }
    names(headers) <- files
    headers <- headers %>%
        bind_rows(.id = 'FileName')
    return(headers)
}
