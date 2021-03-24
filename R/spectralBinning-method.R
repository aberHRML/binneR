#' @importFrom dplyr group_by summarise arrange inner_join select 
#' @importFrom dplyr left_join filter distinct rename vars all_of
#' @importFrom dplyr group_by_at
#' @importFrom tibble tibble deframe
#' @importFrom purrr map
#' @importFrom tidyr spread
#' @importFrom stringr str_c

setMethod("spectralBinning", 
					signature = "Binalysis",
					function(x,verbose = TRUE){
						
						info <- sampleInfo(x)
						files <- filePaths(x)
						
						if (isTRUE(verbose)) message('Reading raw data')
						pks <- getPeaks(files,scans(x))
						
						if (isTRUE(verbose)) message('Gathering bins')
						bin_list <- calcBinList(pks)
						
						if (isTRUE(verbose)) message('Removing single scan events')
						pks <- pks %>% 
							inner_join(bin_list,by = c("polarity", "bin"))
						
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
						binned_data <- pks %>%
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
						
						if (isTRUE(verbose)) message('Calculating bin metrics')
						bin_measures <- calcBinMeasures(pks,
																					 cls)
						
						if (isTRUE(verbose)) message('Calculating accurate m/z')
						accurate_mz <- pks %>%
							group_by_at(vars(all_of(c('fileName',cls,'polarity','bin')))) %>%
							filter(intensity == max(intensity)) %>%
							arrange(bin) %>%
							left_join(bin_measures,by = c('fileName',cls, "polarity", "bin")) %>%
							ungroup()
						
						mz <- accurate_mz %>%
							select(polarity,bin,mz,intensity) %>%
							group_by(polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							select(-intensity) %>%
							mutate(mz = str_c(polarity,mz)) %>%
							ungroup() %>% 
							distinct()
						
						if (isTRUE(verbose)) message('Building intensity matrix')
						binned_data <- binned_data %>%
							left_join(mz,by = c("polarity", "bin")) %>%
							select(-bin) %>%
							split(.$polarity) %>%
							future_map(~{
								.x %>%
									spread(mz,intensity,fill = 0) %>%
									select(-fileName,-polarity)
							})
						
						if (isTRUE(verbose)) message('Gathering file headers')
						headers <- getHeaders(files)
						
						binnedData(x) <- binned_data
						accurateData(x) <- accurate_mz
						spectra(x) <- list(headers = headers, 
															 fingerprints = pks)
						
						return(x)
					}
)

setMethod('ss',signature = 'Binalysis',
					function(x,verbose){
						
						file <- filePaths(x)
						class <- cls(x)
						
						if (length(class) == 0){
							class <- NA
						}
						
						if (isTRUE(verbose)) message('Reading raw data')
						pks <- getPeaks(file,scans(x)) %>%
							mutate(fileName = str_c('Scan ',scan))
						
						if (isTRUE(verbose)) message('Calculating bins')
						bin_list <- calcBinList(pks)
						
						if (isTRUE(verbose)) message('Removing single scan events')
						pks <- pks %>% 
							inner_join(bin_list,by = c("polarity", "bin")) %>%
							mutate(class = class)
						
						if (isTRUE(verbose)) message('Calculating intensity totals')
						binned_data <- pks %>%
							split(.$fileName) %>%
							future_map(~{
								.x %>%
									group_by(fileName,polarity,bin) %>%
									summarise(intensity = sum(intensity))
							}) %>%
							bind_rows()
						
						if (isTRUE(verbose)) message('Calculating bin measures')
						bin_measures <- calcBinMeasures(pks,
																					 'class')
						
						if (isTRUE(verbose)) message('Calculating accurate m/z')
						accurate_mz <- pks %>%
							group_by(fileName,scan,class,polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							arrange(bin)
						
						accurate_mz <- accurate_mz %>%
							left_join(bin_measures,by = c('fileName',"class", "polarity", "bin")) %>%
							ungroup() %>%
							select(scan,polarity,bin,mz,intensity,purity,centrality)
						
						mz <- accurate_mz %>%
							group_by(polarity,bin) %>%
							filter(intensity == max(intensity)) %>%
							select(polarity,bin,mz)
						
						if (isTRUE(verbose)) message('Building intensity matrix')
						binned_data <- binned_data %>%
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
						binnedData(x) <- binned_data
						accurateData(x) <- accurate_mz %>%
							ungroup()
						spectra(x) <- list(headers = headers, fingerprints = pks %>%
															 	ungroup()
						)
						return(x)
					}
)
