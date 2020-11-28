
calcBinList <- function(pks){
        bins <- pks %>%
                group_by(fileName,polarity,scan,bin) %>%
                summarise(intensity = sum(intensity)) %>%
                group_by(polarity,bin) %>%
                summarise(count = n()) %>%
                filter(count > 1) %>%
                select(-count)
}

calcBinMeasures <- function(pks,cls,nCores,clusterType){
        
        binMeasures <- pks %>%
                group_by_at(vars(all_of(c('fileName',
                                          cls,
                                          'polarity',
                                          'bin')))) %>%
                summarise(purity = binPurity(mz,intensity),
                          centrality = binCentrality(mz,intensity))
        
        return(binMeasures)
}
