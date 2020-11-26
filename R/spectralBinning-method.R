#' @importFrom dplyr group_by summarise arrange inner_join select 
#' @importFrom dplyr left_join filter distinct rename vars all_of
#' @importFrom dplyr group_by_at
#' @importFrom tibble tibble deframe
#' @importFrom purrr map
#' @importFrom tidyr spread
#' @importFrom stringr str_c

setMethod("spectralBinning", 
					signature = "Binalysis",
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
							cls <- cls(parameters)
							classes <- info %>%
								select(fileName,all_of(cls(parameters)))
						} else {
							cls <- 'class'
							classes <- info %>%
								select(fileName) %>%
								mutate(class = NA)
						}
						
						nScans <- scans(parameters) %>%
							unique() %>%
							length()
						
						clus <- makeCluster(nSlaves,type = clusterType(parameters))
						
						binnedData <- pks %>%
							split(.$fileName) %>%
							parLapply(clus,.,function(x,nScans){
								
								`%>%` <- getFromNamespace('%>%','magrittr')
								
								x %>%
									split(stringr::str_c(.$fileName,.$polarity,.$bin,.$scan)) %>%
									purrr::map(~{
										tibble(fileName = .x$fileName[1],
													 polarity = .x$polarity[1],
													 bin = .x$bin[1],
													 scan = .x$scan[1],
													 intensity = sum(.x$intensity))
									}) %>%
									dplyr::bind_rows() %>%
									split(stringr::str_c(.$fileName,.$polarity,.$bin)) %>%
									purrr::map(~{
										tibble(fileName = .x$fileName[1],
													 polarity = .x$polarity[1],
													 bin = .x$bin[1],
													 intensity = sum(.x$intensity)/nScans)
									}) %>%
									dplyr::bind_rows()
							},nScans = nScans) %>%
							bind_rows()
						
						pks <- pks %>%
							left_join(classes,by = "fileName") %>%
							split(.$fileName) %>%
							parLapply(clus,.,function(x,nScans){
								
								`%>%` <- getFromNamespace('%>%','magrittr')
								x %>%
									split(stringr::str_c(.$fileName,.[,cls],.$polarity,.$mz,.$bin)) %>%
									purrr::map(~{
										d <- tibble(fileName = .x$fileName[1],
													 polarity = .x$polarity[1],
													 bin = .x$bin[1],
													 intensity = sum(.x$intensity)/nScans)
										d[,cls] <- cls
										return(.x)
									}) %>%
									dplyr::bind_rows() %>%
									dplyr::select(fileName,dplyr::all_of(cls),polarity,mz,bin,intensity)
							},nScans = nScans) %>%
							bind_rows()
						
						stopCluster(clus)
						
						binMeasures <- calcBinMeasures(pks,
																					 cls,
																					 parameters@nCores,
																					 parameters@clusterType)
						
						accurateMZ <- pks %>%
							group_by_at(vars(all_of(c('fileName',cls,'polarity','bin')))) %>%
							filter(intensity == max(intensity)) %>%
							arrange(bin) %>%
							left_join(binMeasures,by = c('fileName',cls, "polarity", "bin"))
						
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
							dplyr::ungroup() %>%
							split(.$polarity) %>%
							parLapply(clus,.,function(x){
								
								`%>%` <- getFromNamespace('%>%','magrittr')
								
								x$mz <- stringr::str_c(x$polarity,x$mz)
								
								x %>%
									tidyr::spread(mz,intensity,fill = 0) %>%
									dplyr::select(-fileName,-polarity)
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
							mutate(fileName = str_c('Scan ',scan))
						
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
							split(.$fileName) %>%
							parLapply(clus,.,function(x){
								`%>%` <- getFromNamespace('%>%','magrittr')
								
								x %>%
									split(stringr::str_c(.$fileName,.$polarity,.$bin)) %>%
									purrr::map(~{
										tibble(fileName = .x$fileName[1],
													 polarity = .x$polarity[1],
													 bin = .x$bin[1],
													 intensity = sum(.x$intensity))
									}) %>%
									dplyr::bind_rows() %>%
									dplyr::select(fileName,polarity,bin,intensity) %>%
									dplyr::distinct()
							}) %>%
							bind_rows()
						
						stopCluster(clus)
						
						binMeasures <- calcBinMeasures(pks,
																					 'class',
																					 parameters@nCores,
																					 parameters@clusterType)
						
						accurateMZ <- pks %>%
							group_by(fileName,scan,class,polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							arrange(bin)
						
						accurateMZ <- accurateMZ %>%
							left_join(binMeasures,by = c('fileName',"class", "polarity", "bin")) %>%
							ungroup() %>%
							select(scan,polarity,bin,mz,intensity,purity,centrality)
						
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
							ungroup() %>%
							split(.$polarity) %>%
							parLapply(clus,.,function(x){
								`%>%` <- getFromNamespace('%>%','magrittr')
								
								x$mz <- stringr::str_c(x$polarity,x$mz)
								x %>%
									tidyr::spread(mz,intensity,fill = 0) %>%
									dplyr::select(-fileName,-polarity)
							})
						
						stopCluster(clus)
						
						headers <- getHeaders(file,parameters@nCores,parameters@clusterType)
						
						x@binParameters@cls <- 'scan'
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