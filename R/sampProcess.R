#  process a single data file
# @name sampProcess
# @description Apply spectral binning on a single data file.
# @param file File path of converted data file
# @param scans A vector of scan numbers that should be retrieved 
# @param dp An integer denoting the number of decimal places for spectral binning
# @param sranges A list of vectors containing the scan events present 
# @param modes A vector of strings denoting mode names
# @return A list containing peak lists for the relevant scans with combined scan ranges for each present mode in the data file. 
# @author Jasen Finch
#' @importFrom mzR openMSfile peaks
#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom magrittr %>%

sampProcess <- function(file,scans,dp,sranges,modes){
	aa <- openMSfile(file)
	pl <- peaks(aa) # obtain peak lists
	pl <- combScans(pl,scans,sranges,modes)
	pl <- lapply(pl,bind_rows,.id = 'Scan')
	pl <- bind_rows(pl,.id = 'Mode')
	pl$mz <- round(pl$mz,5)
	# round and aggregate within scans
	pl$mz <- round(pl$mz,dp)
	pl <- pl %>% group_by(Mode,Scan,mz) %>% summarise(intensity = sum(intensity)) 
	# average the scans
	Nscans <- length(unique(pl$Scan))
	pl <- pl %>% group_by(Mode,mz) %>% summarise(intensity = sum(intensity)/Nscans) 
	return(pl)
}