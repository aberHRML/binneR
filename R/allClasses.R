#' Spectral binning parameters class
#' @description An S4 class to store spectral binning parameters.
#' @slot scans numeric vector containing the scan indexes to use for binning
#' @slot cls the column of class labels to use for aggregating accurate 
#' mass data. Defaults to NULL where accurate mass data will be averaged 
#' across all samples
#' @seealso \code{\link{binParameters}}
#' @export

setClass('BinParameters',
         slots = list(
             scans = 'numeric',
             cls = 'character'
         ),
)

#' Spectral binning analysis class
#' @description An S4 class to store spectrally binned data and accurate 
#' mass information.
#' @slot version package version
#' @slot creation_date creation date
#' @slot file_paths file paths for raw data
#' @slot sample_info tibble containing runinfo data
#' @slot binned_data list containing tibbles of spectrally binned data 
#' for each acquisition mode
#' @slot accurate_mz tibble containin accurate mass information
#' @slot spectra list containing tibbles of headers and class master 
#' mix fingerprints
#' @seealso \code{\link{binneRlyse}} 
#' @export

setClass('Binalysis',
         slots = list(
             version = 'character',
             creation_date = 'character',
             file_paths = 'character',
             sample_info = 'tbl_df',
             binned_data = 'list',
             accurate_mz = 'tbl_df',
             spectra = 'list'
         ),
				 contains = 'BinParameters',
				 prototype = list(
				 	version = packageVersion('binneR') %>%
				 		as.character(),
				 	creation_date = date(),
				 	sample_info = tibble(
				 		fileOrder = character(),
				 		injOrder = numeric(),
				 		fileName = character(),
				 		batch = numeric(),
				 		block = numeric(),
				 		name = character(),
				 		class = character()
				 	),
				 	accurate_mz = tibble()
				 )
)

setValidity('Binalysis',function(object){
	necessary_names <- c('fileOrder','injOrder','fileName','batch','block','name','class')
	
	info_names <- object %>%
		sampleInfo() %>%
		colnames()
	
	presence <- necessary_names %in% info_names
	
	if (FALSE %in% presence) {
		str_c('Sample information should contain the following column names: ',
					str_c(necessary_names,collapse = ', '),
					'.')
	} else {
		TRUE
	}
})

setValidity('Binalysis',function(object){
	file_path_names <- object %>%
		filePaths() %>%
		basename()
	
	info_file_names <- object %>%
		sampleInfo() %>%
		.$fileName
	
	matching <- file_path_names == info_file_names
	
	if (FALSE %in% matching) {
		'File names in paths do not match file names in the sample information.'
	} else {
		TRUE
	}
	
	
})