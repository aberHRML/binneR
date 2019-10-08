
calcBinMeasures <- function(pks,nCores,clusterType){
    
        binMeasures <- pks %>%
            group_by(class,polarity,bin) %>%
            summarise(Purity = binPurity(mz,intensity),Centrality = binCentrality(mz,intensity))

    return(binMeasures)
}
