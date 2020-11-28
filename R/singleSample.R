#' Perform single sample spectral binning
#' @description Perform spectral binning on a single sample.
#' @param file file path
#' @param class optional class name
#' @param verbose show console output
#' @examples 
#' filePath <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]
#' 
#' bd <- singleSample(filePath)
#' @importFrom utils capture.output
#' @export

singleSample <- function(file, 
												 class = NA, 
												 verbose = TRUE){

	if (length(file) > 1) {
		stop('Only suitable for a single file!')	
	}
	
	if (length(class) > 1) {
		stop('Only a single class can be affiliated!')
	}
	
	parameters <- detectParameters(file)
	
	i <- tibble(fileOrder = seq_len(length(scans(parameters))),
							fileName = basename(file),
							injOrder = seq_len(length(scans(parameters))),
							name = str_c('Scan ',scans(parameters)),
							class = class,
							batch = 1,
							block = 1)
	
	if (!is.na(class)) {
		cls(parameters) <- class	
	}
	
	pv <- packageVersion('binneR') %>% as.character()
	
	if (verbose == TRUE) {
		startTime <- proc.time()
		message(str_c('\n',blue('binneR'),red(str_c('v',pv)),date(),sep = ' '))		
		message(str_c(str_c(rep('_',console_width()),collapse = ''),sep = ''))
		params <- parameters %>%
			{capture.output(print(.))} %>%
			{.[-1]} %>%
			str_c(collapse = '\n')
		message(params)
		message(str_c(str_c(rep('_',console_width()),collapse = ''),'\n',sep = ''))
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
	
	if (verbose == TRUE) {
		endTime <- proc.time()
		ellapsed <- {endTime - startTime} %>%
			.[3] %>%
			round(1) %>%
			seconds_to_period() %>%
			str_c('[',.,']')
		message(str_c(green('Completed! '),ellapsed,sep = ''))
	}
	
	return(x)
}