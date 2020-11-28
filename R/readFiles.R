#' Read and process mulitple data files
#' @name readFiles
#' @description Apply spectral binning on multiple data files.
#' @param files A vector of converted data file paths
#' @param dp An integer denoting the number of decimal places for spectral 
#' binning
#' @param scans A vector of scan numbers that should be retrieved 
#' @return A list containing peak lists for the relevant scans with combined 
#' scan ranges for each present mode in the data file. 
#' @details 
#' Parallel processing is managed by the \code{future} package. This can be specified using the \code{plan() function}. See the example below and \code{?future::plan} for details on how this can be specified.
#' @author Jasen Finch
#' @examples 
#' ## Example file path
#' file_paths <- metaboData::filePaths('FIE-HRMS',
#'                                     'BdistachyonEcotypes')[1]
#' 
#' ## Optionally declare parallel processing backend
#' # plan(future::multisession,workers = 2)
#'                                                                         
#' ## Process example file
#' res <- readFiles(file_paths,
#'                  dp = 2,
#'                  scans = 6:17)
#'
#' @importFrom furrr future_map
#' @importFrom dplyr bind_rows 
#' @importFrom tidyr spread
#' @export

readFiles <- function(files,
											dp, 
											scans){ 
	
	pl <- future_map(files,
										sampProcess,
										scans = scans,
										dp = dp) %>%
		set_names(files) %>%
		bind_rows(.id = 'file') %>%
		mutate(mz = str_c(polarity,mz)) %>%
		split(.$polarity) %>%
		future_map(~{
			
			.x %>%
				spread(key = 'mz',value = 'intensity',fill = 0) %>%
				ungroup() %>%
				select(-file,-polarity)
		})
	return(pl)
}  
