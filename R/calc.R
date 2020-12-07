
calcBinList <- function(pks){
        bins <- pks %>%
                group_by(fileName,polarity,scan,bin) %>%
                summarise(intensity = sum(intensity),
                          .groups = 'drop') %>%
                group_by(polarity,bin) %>%
                summarise(count = n(),
                          .groups = 'drop') %>%
                filter(count > 1) %>%
                select(-count)
}

calcBinMeasures <- function(pks,cls){
        
        binMeasures <- pks %>%
                group_by_at(vars(all_of(c('fileName',
                                          cls,
                                          'polarity',
                                          'bin')))) %>%
                summarise(purity = binPurity(mz,intensity),
                          centrality = binCentrality(mz,intensity),
                          .groups = 'drop')
        
        return(binMeasures)
}


