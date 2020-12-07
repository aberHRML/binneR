#' @importFrom dplyr group_by summarise arrange inner_join select 
#' @importFrom dplyr left_join filter distinct rename vars all_of
#' @importFrom dplyr group_by_at
#' @importFrom tibble tibble deframe
#' @importFrom purrr map
#' @importFrom tidyr spread
#' @importFrom stringr str_c

setMethod("spectralBinning", 
					signature = "Binalysis",
					function(x,verbose){
						
						info <- sampleInfo(x)
						files <- filePaths(x)
						
						if (isTRUE(verbose)) message('Reading raw data')
						pks <- getPeaks(files,scans(x))
						
						if (isTRUE(verbose)) message('Calculating bins')
						binList <- calcBinList(pks)
						
						if (isTRUE(verbose)) message('Removing single scan events')
						pks <- pks %>% 
							inner_join(binList,by = c("polarity", "bin"))
						
						if (length(cls(x)) > 0) {
							cls <- cls(x)
							classes <- info %>%
								select(fileName,all_of(cls(x)))
						} else {
							cls <- 'class'
							classes <- info %>%
								select(fileName) %>%
								mutate(class = NA)
						}
						
						n_scans <- nScans(x)
						
						if (isTRUE(verbose)) message('Averaging intensities across scans')
						binnedData <- pks %>%
							split(.$fileName) %>%
							future_map(~{
								.x %>%
									group_by(fileName,polarity,bin,scan) %>%
									summarise(intensity = sum(intensity),
														.groups = 'drop')	%>%
									group_by(fileName,polarity,bin) %>%
									summarise(intensity = sum(intensity)/n_scans,
														.groups = 'drop')
							}) %>%
							bind_rows()
						
						pks <- pks %>%
							left_join(classes,by = "fileName") %>%
							split(.$fileName) %>%
							future_map(~{
								.x %>%
									group_by_at(
										vars(
											all_of(c('fileName',
															 cls,
															 'polarity','mz','bin')))) %>%
									summarise(intensity = sum(intensity)/n_scans,
														.groups = 'drop')
							}) %>%
							bind_rows()
						
						if (isTRUE(verbose)) message('Calculating bin measures')
						binMeasures <- calcBinMeasures(pks,
																					 cls)
						
						if (isTRUE(verbose)) message('Calculating accurate m/z')
						accurateMZ <- pks %>%
							group_by_at(vars(all_of(c('fileName',cls,'polarity','bin')))) %>%
							filter(intensity == max(intensity)) %>%
							arrange(bin) %>%
							left_join(binMeasures,by = c('fileName',cls, "polarity", "bin")) %>%
							ungroup()
						
						mz <- accurateMZ %>%
							select(polarity,bin,mz,intensity) %>%
							group_by(polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							select(-intensity) %>%
							mutate(mz = str_c(polarity,mz)) %>%
							ungroup()
						
						if (isTRUE(verbose)) message('Building intensity matrix')
						binnedData <- binnedData %>%
							left_join(mz,by = c("polarity", "bin")) %>%
							select(-bin) %>%
							split(.$polarity) %>%
							future_map(~{
								.x %>%
									spread(mz,intensity,fill = 0) %>%
									select(-fileName,-polarity)
							})
						
						headers <- getHeaders(files)
						
						binnedData(x) <- binnedData
						accurateData(x) <- accurateMZ
						spectra(x) <- list(headers = headers, 
															 fingerprints = pks)
						
						return(x)
					}
)

setMethod('ss',signature = 'Binalysis',
					function(x){
						
						file <- filePaths(x)
						class <- cls(x)
						
						if (length(class) == 0){
							class <- NA
						}
						
						pks <- getPeaks(file,scans(x)) %>%
							mutate(fileName = str_c('Scan ',scan))
						
						binList <- calcBinList(pks)
						
						pks <- pks %>% 
							inner_join(binList,by = c("polarity", "bin")) %>%
							mutate(class = class)
						
						binnedData <- pks %>%
							split(.$fileName) %>%
							future_map(~{
								.x %>%
									group_by(fileName,polarity,bin) %>%
									summarise(intensity = sum(intensity))
							}) %>%
							bind_rows()
						
						binMeasures <- calcBinMeasures(pks,
																					 'class')
						
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
						
						binnedData <- binnedData %>%
							left_join(mz,by = c("polarity", "bin")) %>%
							select(-bin) %>%
							ungroup() %>%
							split(.$polarity) %>%
							future_map(~{
								.x %>%
									ungroup() %>%
									mutate(mz = str_c(polarity,mz)) %>%
									spread(mz,intensity,fill = 0) %>%
									select(-fileName,-polarity)
							})
						
						headers <- getHeaders(file)
						
						cls(x) <- 'scan'
						binnedData(x) <- binnedData
						accurateData(x) <- accurateMZ %>%
							ungroup()
						spectra(x) <- list(headers = headers, fingerprints = pks %>%
															 	ungroup()
						)
						return(x)
					}
)
