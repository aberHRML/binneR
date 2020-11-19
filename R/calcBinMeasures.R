
calcBinMeasures <- function(pks,nCores,clusterType){
    
        binMeasures <- pks %>%
            group_by(class,polarity,bin) %>%
            summarise(purity = binPurity(mz,intensity),
            					centrality = binCentrality(mz,intensity))

    return(binMeasures)
}
