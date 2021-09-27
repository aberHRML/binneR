#' Perform spectral binning
#' @description perform spectral binning.
#' @param files character vector of file paths to use for spectral binning
#' @param info tibble containing sample information
#' @param parameters object of class BinParameters containing parameters 
#' for spectral binning
#' @param verbose show console output
#' @return S4 object of class Binalysis.
#' @details 
#' Parallel processing is managed by the \code{future} package. This can 
#' be specified using the \code{plan() function}. See the example below 
#' and \code{?future::plan} for details on how this can be specified.
#' 
#' By default, spectral binning is performed at the recommended 2 decimal 
#' places. This can be altered by setting either the global option 
#' \code{binner_dp} or the environment variable \code{BINNER_DP}.
#' 
#' @seealso \code{\link{Binalysis-class}}, \code{\link{binParameters}}, 
#' \code{\link{sampleInfo}}, \code{\link{binnedData}},  \code{\link{accurateData}}
#' @examples 
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonTechnical')
#' 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonTechnical')
#' 
#' parameters <- detectParameters(files)
#' cls(parameters) <- 'class'
#' 
#' ## Optionally declare parallel processing backend
#' # plan(future::multisession,workers = 2)
#' 
#' analysis <- binneRlyse(files, 
#'                        info, 
#'                        parameters = parameters)
#' @importFrom dplyr ungroup n
#' @importFrom magrittr %>%
#' @importFrom crayon blue red green
#' @importFrom cli console_width
#' @importFrom utils packageVersion
#' @importFrom lubridate seconds_to_period
#' @export

binneRlyse <- function(files, 
																							info, 
																							parameters = binParameters(), 
																							verbose = TRUE){
	
	x <- new('Binalysis',
										parameters,
										creation_date = date(),
										file_paths = files,
										sample_info = info)
	
	if (verbose == TRUE) {
		startTime <- proc.time()
		message(str_c(blue('binneR'),
																red(str_c('v',
																										version(x))),
																creationDate(x),
																sep = ' '))		
		message(str_c(str_c(rep('_',console_width()),collapse = ''),sep = ''))
		params <- parameters %>%
			{capture.output(print(.))} %>%
			{.[-1]} %>%
			str_c(collapse = '\n')
		message(params)
		message(str_c(str_c(rep('_',console_width()),collapse = ''),'\n',sep = ''))
	}
	
	x <- x %>% 
		spectralBinning(verbose = verbose)
	
	if (verbose == TRUE) {
		message()
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
