#' @importFrom dplyr group_by summarise arrange inner_join select 
#' @importFrom dplyr left_join filter distinct rename
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom tidyr spread
#' @importFrom stringr str_c

setMethod("spectralBinning", signature = "Binalysis",
					function(x){
						
						parameters <- x@binParameters
						
						info <- info(x)
						files <- x@files
						
						nSlaves <- ceiling(length(files) / 20)
						
						if (nSlaves > nCores(parameters)) {
							nSlaves <- nCores(parameters)
						}
						
						pks <- getPeaks(files,scans(parameters),
														nSlaves,
														clusterType(parameters))
						
						binList <- calcBinList(pks)
						
						pks <- pks %>% 
							inner_join(binList,by = c("polarity", "bin"))
						
						if (length(cls(parameters)) > 0) {
							classes <- info %>%
								select(all_of(cls(parameters))) %>%
								deframe()
						} else {
							classes <- rep(NA,nrow(info))
						}
						
						cls <- tibble(fileName = files, class = classes) 
						
						nScans <- pks$scan %>%
							unique() %>%
							length()
						
						clus <- makeCluster(nSlaves,type = parameters@clusterType)
						
						binnedData <- pks %>%
							split(.$fileName) %>%
							parLapply(clus,.,function(x,nScans){
								x %>%
									group_by(fileName,polarity,bin,scan) %>%
									summarise(intensity = sum(intensity))	%>%
									group_by(fileName,polarity,bin) %>%
									summarise(intensity = sum(intensity)/nScans)
							},nScans = nScans) %>%
							bind_rows()
						
						pks <- pks %>%
							left_join(cls,by = "fileName") %>%
							split(.$fileName) %>%
							parLapply(clus,.,function(x,nScans){
								x %>%
									group_by(class,fileName,polarity,mz,bin) %>%
									summarise(intensity = sum(intensity)/nScans)
							},nScans = nScans) %>%
							bind_rows()
						
						stopCluster(clus)
						
						pks <- pks %>%
							group_by(class,polarity,mz,bin) %>%
							summarise(intensity = sum(intensity),.groups = 'drop')
						
						binMeasures <- calcBinMeasures(pks,
																					 parameters@nCores,
																					 parameters@clusterType)
						
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
									select(-fileName,-polarity)
							})
						
						stopCluster(clus)
						
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
						
						binMeasures <- calcBinMeasures(pks,
																					 parameters@nCores,
																					 parameters@clusterType)
						
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