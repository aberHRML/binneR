
calcBinList <- function(pks){
 bins <- pks %>%
     group_by(fileName,polarity,scan,bin) %>%
         summarise(intensity = sum(intensity)) %>%
     group_by(polarity,bin) %>%
     summarise(count = n()) %>%
     filter(count > 1) %>%
     select(-count)
}
