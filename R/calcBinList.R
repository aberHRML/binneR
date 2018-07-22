
calcBinList <- function(pks){
 bins <- pks %>%
     group_by(File,Mode,Scan,Bin) %>%
         summarise(intensity = sum(intensity)) %>%
     group_by(Mode,Bin) %>%
     summarise(Count = n()) %>%
     filter(Count > 1) %>%
     select(-Count)
}
