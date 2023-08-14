
calcBinList <- function(pks){
  bins <- pks %>%
    group_by(idx,fileName,polarity,scan,bin) %>%
    summarise(intensity = sum(intensity),
              .groups = 'drop') %>%
    group_by(polarity,bin) %>%
    summarise(count = n(),
              .groups = 'drop') %>%
    filter(count > 1) %>%
    select(-count)
}

calcBinMeasures <- function(pks,cls){
  
  dp <- binnerDP()
  
  binMeasures <- pks %>%
    group_by_at(vars(all_of(c('idx',
    																										'fileName',
                              cls,
                              'polarity',
                              'bin')))) %>%
    summarise(purity = binPurity(mz,
                                 intensity,
                                 dp = dp),
              centrality = binCentrality(mz,
                                         intensity,
                                         dp = dp),
              .groups = 'drop')
  
  return(binMeasures)
}

setMethod('nScans',signature = 'Binalysis',
          function(x){
            x %>%
              scans() %>%
              unique() %>%
              length()
          })
