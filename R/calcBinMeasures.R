
calcBinMeasures <- function(pks,nCores,clusterType){
    
        binMeasures <- pks %>%
            group_by(Class,Mode,Bin) %>%
            summarise(Purity = binPurity(mz,intensity),Centrality = binCentrality(mz,intensity))
    
    # if (nCores > 1) {
    #     binMeasures <- pks %>%
    #         split(.$Mode) %>%
    #         map(~{
    #             d <- .
    #             d %>%
    #                 split(.$Mode) %>%
    #                 map(~{
    #                     d <- .
    #                     clus <- makeCluster(parameters@nCores,type = parameters@clusterType)
    #                     d <- d %>%
    #                         split(.$Bin) %>%
    #                         parLapply(cl = clus, fun = function(x){
    #                             tibble(Purity = binPurity(x$mz,x$intensity),
    #                                    Centrality = binCentrality(x$mz,x$intensity))
    #                         }) %>%
    #                         bind_rows()
    #                     stopCluster(clus)
    #                     return(d)
    #                 }) %>%
    #                 bind_rows()
    #         }) %>%
    #         bind_rows()
    # } else {
    #     binMeasures <- masterMat %>%
    #         group_by(Class,Mode,Bin) %>%
    #         summarise(Purity = binPurity(mz,intensity),Centrality = binCentrality(mz,intensity))
    # }
    return(binMeasures)
}
