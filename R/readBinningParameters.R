#' Read binning parameters from file
#' @description Parse binning parameters from a YAML formatted file.
#' @param file file path
#' @examples 
#' file <- system.file('binning_parameters.yml',package = 'binneR')
#' parameters <- readBinningParameters(file)
#' @importFrom yaml read_yaml
#' @importFrom stringr str_detect regex
#' @export

readBinningParameters <- function(file){
	
	parameters <- read_yaml(file)
	
	if ('scans' %in% names(parameters)) {
		parameters$scans <- checkScans(parameters$scans)
	}
	
	bp <- do.call(binParameters,parameters)
	
	return(bp)
}

checkScans <- function(scan_range){
	if ((!str_detect(scan_range,regex('c\\((.*?)\\)'))) &
			(!str_detect(scan_range,regex('[0-9]+:[0-9]+')))){
		
		stop(str_c('Field "scans" should be a either a numeric vector ',
							 '(eg. "c(1,3)") or numeric range (eg. "5:12").'),
				 call. = FALSE)
		
	}
	
	scan_range <- eval(parse(text = scan_range))
	
	return(scan_range)
} 
