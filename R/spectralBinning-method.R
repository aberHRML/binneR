#' @importFrom dplyr group_by summarise arrange inner_join select left_join filter distinct rename
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom tidyr spread
#' @importFrom stringr str_c

setMethod("spectralBinning", signature = "Binalysis",
					function(x){
						
						parameters <- x@binParameters
						
						info <- x@info
						files <- x@files
						
						nSlaves <- ceiling(length(files) / 20)
						
						if (nSlaves > nCores(parameters)) {
							nSlaves <- nCores(parameters)
						}
						
						pks <- getPeaks(files,parameters@scans,
														nSlaves,
														parameters@clusterType)
						
						binList <- calcBinList(pks)
						
						pks <- pks %>% 
							inner_join(binList,by = c("polarity", "bin"))
						
						if (length(parameters@cls) != 0) {
							classes <- unlist(info[,parameters@cls],use.names = F)
						} else {
							classes <- rep(1,nrow(info))
						}
						
						cls <- tibble(file = files, class = classes) 
						
						nScans <- pks$scan %>%
							unique() %>%
							length()
						
						clus <- makeCluster(nSlaves,type = parameters@clusterType)
						
						binnedData <- pks %>%
							split(.$file) %>%
							parLapply(clus,.,function(x,nScans){
								x %>%
									group_by(file,polarity,bin,scan) %>%
									summarise(intensity = sum(intensity))	%>%
									group_by(file,polarity,bin) %>%
									summarise(intensity = sum(intensity)/nScans)
							},nScans = nScans) %>%
							bind_rows()
						
						pks <- pks %>%
							left_join(cls,by = "file") %>%
							split(.$file) %>%
							parLapply(clus,.,function(x,nScans){
								x %>%
									group_by(class,file,polarity,mz,bin) %>%
									summarise(intensity = sum(intensity)/nScans)
							},nScans = nScans) %>%
							bind_rows()
						
						stopCluster(clus)
						
						classes <- pks %>%
							ungroup() %>%
							select(class,file) %>%
							distinct() %>%
							.$class %>%
							table() %>%
							as_tibble() %>%
							rename(class = '.')
						
						pks <- pks %>%
							split(.$class)
						
						nSlaves <- ceiling(length(pks))
						
						if (nSlaves > nCores(parameters)) {
							nSlaves <- nCores(parameters)
						}
						
						clus <- makeCluster(nSlaves,type = parameters@clusterType)
						
						pks  <- parLapply(clus,classes$class,function(x,classes,pks){
							cls <- x
							nCls <- classes$n[classes$class == cls]
							pks[[cls]] %>%
								filter(class == cls) %>%
								group_by(class,polarity,mz,bin) %>%
								summarise(intensity = sum(intensity)/nCls)
						},classes = classes,pks = pks) %>%
							bind_rows()
						
						stopCluster(clus)
						
						binMeasures <- calcBinMeasures(pks,parameters@nCores,parameters@clusterType)
						
						accurateMZ <- pks %>%
							group_by(class,polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							arrange(bin)
						
						accurateMZ <- accurateMZ %>%
							left_join(binMeasures,by = c("class", "polarity", "bin"))
						
						mz <- accurateMZ %>%
							group_by(polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							select(polarity,bin,mz)
						
						nSlaves <- ceiling(length(accurateMZ$polarity %>%
																				unique()))
						
						if (nSlaves > nCores(parameters)) {
							nSlaves <- nCores(parameters)
						}
						
						clus <- makeCluster(nSlaves,type = parameters@clusterType)
						
						binnedData <- binnedData %>%
							left_join(mz,by = c("polarity", "bin")) %>%
							select(-bin) %>%
							split(.$polarity) %>%
							parLapply(clus,.,function(x){
								x %>%
									ungroup() %>%
									mutate(mz = str_c(polarity,mz)) %>%
									spread(mz,intensity,fill = 0) %>%
									select(-file,-polarity)
							})
						
						stopCluster(clus)
						
						if (length(parameters@cls) == 0) {
							accurateMZ$class <- NA
							pks$class <- NA
						}
						
						headers <- getHeaders(files,parameters@nCores,parameters@clusterType)
						
						x@binLog <- date()
						x@info <- info
						x@binnedData <- binnedData
						x@accurateMZ <- accurateMZ %>%
							ungroup()
						x@spectra <- list(headers = headers, fingerprints = pks %>%
																ungroup()
						)
						return(x)
					}
)

setMethod('ss',signature = 'Binalysis',
					function(x){
						
						file <- x@files
						class <- x@info$class[1]
						parameters <- x@binParameters
						
						pks <- getPeaks(file,scans(parameters),
														1,
														clusterType(parameters)) %>%
							mutate(file = str_c('Scan ',scan))
						
						binList <- calcBinList(pks)
						
						pks <- pks %>% 
							inner_join(binList,by = c("polarity", "bin")) %>%
							mutate(class = class)
						
						nSlaves <- ceiling(length(scans(parameters)) / 20)
						
						if (nSlaves > nCores(parameters)) {
							nSlaves <- nCores(parameters)
						}
						
						clus <- makeCluster(nSlaves,type = parameters@clusterType)
						
						binnedData <- pks %>%
							split(.$file) %>%
							parLapply(clus,.,function(x){
								x %>%
									group_by(file,polarity,bin) %>%
									summarise(intensity = sum(intensity))
							}) %>%
							bind_rows()
						
						stopCluster(clus)
						
						pks  <- pks %>%
							group_by(class,polarity,mz,bin) %>%
							summarise(intensity = sum(intensity))
						
						binMeasures <- calcBinMeasures(pks,parameters@nCores,parameters@clusterType)
						
						accurateMZ <- pks %>%
							group_by(class,polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							arrange(bin)
						
						accurateMZ <- accurateMZ %>%
							left_join(binMeasures,by = c("class", "polarity", "bin"))
						
						mz <- accurateMZ %>%
							group_by(polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							select(polarity,bin,mz)
						
						nSlaves <- ceiling(length(accurateMZ$polarity %>%
																				unique()))
						
						if (nSlaves > nCores(parameters)) {
							nSlaves <- nCores(parameters)
						}
						
						clus <- makeCluster(nSlaves,type = parameters@clusterType)
						
						binnedData <- binnedData %>%
							left_join(mz,by = c("polarity", "bin")) %>%
							select(-bin) %>%
							split(.$polarity) %>%
							parLapply(clus,.,function(x){
								x %>%
									ungroup() %>%
									mutate(mz = str_c(polarity,mz)) %>%
									spread(mz,intensity,fill = 0) %>%
									select(-file,-polarity)
							})
						
						stopCluster(clus)
						
						headers <- getHeaders(file,parameters@nCores,parameters@clusterType)
						
						x@binLog <- date()
						x@binnedData <- binnedData
						x@accurateMZ <- accurateMZ %>%
							ungroup()
						x@spectra <- list(headers = headers, fingerprints = pks %>%
																ungroup()
						)
						return(x)
					}
)