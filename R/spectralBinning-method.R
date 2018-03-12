#' @importFrom dplyr group_by summarise arrange inner_join select left_join filter distinct rename
#' @importFrom tibble tibble
#' @importFrom tidyr spread
#' @importFrom stringr str_c

setMethod("spectralBinning", signature = "Binalysis",
          function(x){
              
              parameters <- x@binParameters

              info <- x@info
              files <- x@files
              
              pks <- getPeaks(files,parameters@scans,parameters@sranges,parameters@modes,parameters@nCores,parameters@clusterType)
             
              binList <- calcBinList(pks)
              
              pks <- pks %>% 
                  inner_join(binList,by = c("Mode", "Bin"))
              
              if (length(parameters@cls) != 0) {
                  classes <- unlist(info[,parameters@cls],use.names = F)
              } else {
                  classes <- rep(1,nrow(info))
              }
              
              cls <- tibble(File = files, Class = classes) 
              
              nScans <- pks$Scan %>%
                  unique() %>%
                  length()
              
              binnedData <- pks %>%
                  group_by(File,Mode,Bin,Scan) %>%
                  summarise(intensity = sum(intensity)) %>%
                  group_by(File,Mode,Bin) %>%
                  summarise(intensity = sum(intensity)/nScans)
              
              pks <- pks %>%
                  left_join(cls,by = "File") %>%
                  group_by(Class,File,Mode,mz,Bin) %>%
                  summarise(intensity = sum(intensity)/nScans)
              
              classes <- pks %>%
                  tbl_df() %>%
                  select(Class,File) %>%
                  distinct() %>%
                  .$Class %>%
                  table() %>%
                  as_tibble() %>%
                  rename(Class = '.')
              
              pks <- pks %>%
                  split(.$Class)
              
              pks  <- map(classes$Class,~{
                  cls <- .
                  nCls <- classes$n[classes$Class == cls]
                  pks[[cls]] %>%
                      filter(Class == cls) %>%
                      group_by(Class,Mode,mz,Bin) %>%
                      summarise(intensity = sum(intensity)/nCls)
              }) %>%
                  bind_rows()
              
              binMeasures <- calcBinMeasures(pks,parameters@nCores,parameters@clusterType)
              
              accurateMZ <- pks %>%
                  group_by(Class,Mode,Bin) %>%
                  filter(intensity == max(intensity)) %>%
                  arrange(Bin)
              
              accurateMZ <- accurateMZ %>%
                  left_join(binMeasures,by = c("Class", "Mode", "Bin"))
              
              mz <- accurateMZ %>%
                  group_by(Mode,Bin) %>%
                  filter(intensity == max(intensity)) %>%
                  select(Mode,Bin,mz)
              
              binnedData <- binnedData %>%
                  left_join(mz,by = c("Mode", "Bin")) %>%
                  select(-Bin) %>%
              split(.$Mode) %>%
                  map(~{
                      d <- .
                      d %>%
                          tbl_df() %>%
                          spread(mz,intensity,fill = 0) %>%
                          select(-File,-Mode)
                  })
              modes <- names(binnedData)
              binnedData <- map(modes,~{
                  d <- binnedData[[.]]
                  colnames(d) <- str_c(.,colnames(d))
                  return(d)
              })
              names(binnedData) <- modes
              
              if (length(parameters@cls) == 0) {
                  accurateMZ <- accurateMZ %>%
                      tbl_df() %>%
                      select(-Class)
                  pks <- pks %>%
                      tbl_df() %>%
                      select(-Class)
              }
              
              headers <- getHeaders(files,parameters@nCores,parameters@clusterType)

              
              x@binLog <- date()
              x@info <- info
              x@binnedData <- binnedData
              x@accurateMZ <- accurateMZ
              x@spectra <- list(headers = headers, fingerprints = pks)
              return(x)
          }
)