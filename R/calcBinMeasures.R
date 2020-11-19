
calcBinMeasures <- function(pks,cls,nCores,clusterType){
    
        binMeasures <- pks %>%
            group_by_at(vars(all_of(c('fileName',cls,'polarity','bin')))) %>%
            summarise(purity = binPurity(mz,intensity),
            					centrality = binCentrality(mz,intensity))

    return(binMeasures)
}
