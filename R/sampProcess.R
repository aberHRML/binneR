#' @importFrom mzR openMSfile peaks
#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom magrittr %>%

sampProcess <- function(file,scans,dp){
	aa <- openMSfile(file)
	pl <- getFile(file,scans) %>%
		mutate(mz = round(mz,dp)) %>% 
		group_by(polarity,mz) %>% 
		summarise(intensity = sum(intensity)/length(scans)) 
	# average the scans
	return(pl)
}