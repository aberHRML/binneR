#' singleSample
#' @description perform spectral binning on a single sample
#' @param file file path
#' @param class optional class name
#' @param nCores number of cores to use for parallel processing
#' @param clusterType cluster type to use for parallel processing
#' @param verbose show console output
#' filePath <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]
#' 
#' bd <- singleSample(filePath)
#' @export

singleSample <- function(file, class = NA, nCores = detectCores(), clusterType = detectClusterType(), verbose = T){

	if (length(file) > 1) {
		stop('Only suitable for a single file!')	
	}
	
	if (length(class) > 1) {
		stop('Only a single class can be affiliated!')
	}
	
	parameters <- detectParameters(file,nCores = nCores,clusterType = clusterType)
	
	i <- tibble(fileOrder = 1:length(scans(parameters)),
							fileName = basename(file),
							injOrder = 1:length(scans(parameters)),
							name = str_c('Scan ',scans(parameters)),
							class = class,
							batch = 1,
							block = 1)
	
	if (!is.na(class)) {
		cls(parameters) <- class	
	}
	
	pv <- packageVersion('binneR') %>% as.character()
	
	if (verbose == T) {
		startTime <- proc.time()
		cat('\n',blue('binneR'),red(str_c('v',pv)),date(),'\n')		
		cat(rep('_',console_width()),'\n',sep = '')
		print(parameters)
		cat(rep('_',console_width()),'\n',sep = '')
	}
	
	x <- new('Binalysis',
									binLog = character(),
									binParameters = parameters,
									files = file,
									info = i,
									binnedData = list(),
									accurateMZ = tibble(),
									spectra = list()
	) %>%
		ss()
	
	if (verbose == T) {
		endTime <- proc.time()
		ellapsed <- {endTime - startTime} %>%
			.[3] %>%
			round(1) %>%
			seconds_to_period() %>%
			str_c('[',.,']')
		cat('\n',green('Completed! '),ellapsed,'\n\n',sep = '')
	}
	
	return(x)
}