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

sampProcess <- function(file,scans,dp,sranges,modes){
	aa <- openMSfile(file)
	pl <- peaks(aa) # obtain peak lists
	pl <- combScans(pl,scans,sranges,modes)
	# round and aggregate within scans
	pl <- lapply(pl,function(x,dp){
		return(lapply(x,function(y,dp){
			# round to dp
			y[,'mz'] <- round(y[,'mz'],5)
			y[,"mz"] <- round(y[,"mz"],dp)
			# aggregate bins to give ion totals
			y <- aggregate(y[,"intensity"],list(y[,"mz"]),sum)
			return(y)
			},dp = dp))
		},dp = dp)
	# add zeros for missing bins in each scan
	pl <- lapply(pl,addMasses)
	pl <- lapply(pl,massMat)
	# average the scans
	pl <- lapply(pl,function(x){
		x <- apply(x,2,mean)
		return(x)
		})
	pl <- lapply(pl,function(x){
		x <- data.frame(mz = as.numeric(as.character(names(x))),intensity = x,row.names = NULL)
		return(x)
		})
	names(pl) <- modes
	return(pl)
}