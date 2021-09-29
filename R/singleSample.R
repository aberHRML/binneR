#' Perform single sample spectral binning
#' @description Perform spectral binning on a single sample.
#' @param file file path
#' @param class optional class name
#' @param verbose show console output
#' @seealso \code{\link{Binalysis-class}}
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
#' @examples 
#' \dontrun{
#' file_path <- metaboData::filePaths('FIE-HRMS','BdistachyonTechnical')[1]
#' 
#' ## Optionally declare parallel processing backend
#' # plan(future::multisession,workers = 2)
#' 
#' bd <- singleSample(file_path)
#' }
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
	
	x <-  new('Binalysis',
											parameters,
											creation_date = date(),
											file_paths = file,
											sample_info = i)
	
	if (!is.na(class)) {
		cls(x) <- class	
	}
	
	if (verbose == TRUE) {
		startTime <- proc.time()
		message(str_c(blue('binneR'),red(str_c('v',version(x))),creationDate(x),sep = ' '))		
		message(str_c(str_c(rep('_',console_width()),collapse = ''),sep = ''))
		params <- parameters %>%
			{capture.output(print(.))} %>%
			{.[-1]} %>%
			str_c(collapse = '\n')
		message(params)
		message(str_c(str_c(rep('_',console_width()),collapse = ''),'\n',sep = ''))
	}
	
	x <- x %>%
		ss(verbose = verbose)
	
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
