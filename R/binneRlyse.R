#' binneRlyse
#' @description perform spectral binning.
#' @param files character vector of file paths to use for spectral binning
#' @param info tibble containing sample information
#' @param parameters object of class BinParameters containing parameters for spectral binning
#' @param verbose show console output
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#' @importFrom crayon blue red green
#' @importFrom cli console_width
#' @importFrom utils packageVersion
#' @importFrom lubridate seconds_to_period
#' @examples 
#' \dontrun{
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' analysis <- binneRlyse(files, 
#'                        info, 
#'                        parameters = binParameters(detectInfusionScans(files)))
#'    }
#' @export

binneRlyse <- function(files, info, parameters = binParameters(), verbose = T){
	pv <- packageVersion('binneR') %>% as.character()
	
	if (verbose == T) {
		startTime <- proc.time()
		cat('\n',blue('binneR'),red(str_c('v',pv)),date(),'\n')		
		cat(rep('_',console_width()),'\n',sep = '')
		print(parameters)
		cat(rep('_',console_width()),'\n',sep = '')
	}
	
	analysis <- new('Binalysis',
									binLog = character(),
									binParameters = parameters,
									files = files,
									info = info,
									binnedData = list(),
									accurateMZ = tbl_df(data.frame()),
									spectra = list()
	) %>% spectralBinning()
	
	if (verbose == T) {
		endTime <- proc.time()
		ellapsed <- {endTime - startTime} %>%
			.[3] %>%
			round(1) %>%
			seconds_to_period() %>%
			str_c('[',.,']')
		cat('\n',green('Completed! '),ellapsed,'\n\n',sep = '')
	}
	
	return(analysis)
}