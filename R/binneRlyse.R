#' Perform spectral binning
#' @description perform spectral binning.
#' @param files character vector of file paths to use for spectral binning
#' @param info tibble containing sample information
#' @param parameters object of class BinParameters containing parameters 
#' for spectral binning
#' @param verbose show console output
#' @return S4 object of class Binalysis.
#' @seealso \code{\link{Binalysis-class}}, \code{\link{binParameters}}, \code{\link{info}}, \code{\link{binnedData}},  \code{\link{accurateData}}
#' @examples 
#' \dontrun{
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' parameters <- detectParameters(files)
#' cls(parameters) <- 'class'
#' 
#' analysis <- binneRlyse(files, 
#'                        info, 
#'                        parameters = parameters)
#'    }
#' @importFrom dplyr ungroup n
#' @importFrom magrittr %>%
#' @importFrom crayon blue red green
#' @importFrom cli console_width
#' @importFrom utils packageVersion
#' @importFrom lubridate seconds_to_period
#' @export

binneRlyse <- function(files, info, parameters = binParameters(), verbose = TRUE){
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
	
	analysis <- new('Binalysis',
									binLog = character(),
									binParameters = parameters,
									files = files,
									info = info,
									binnedData = list(),
									accurateMZ = tibble(),
									spectra = list()
	) %>% spectralBinning()
	
	if (verbose == TRUE) {
		endTime <- proc.time()
		ellapsed <- {endTime - startTime} %>%
			.[3] %>%
			round(1) %>%
			seconds_to_period() %>%
			str_c('[',.,']')
		message(str_c(green('Completed! '),ellapsed,sep = ''))
	}
	
	return(analysis)
}