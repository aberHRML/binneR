#' Detect infusion profile scan range
#' @description Detect infusion scans for a set of FIE-MS infusion profiles.
#' @param files character vector of file paths to use
#' @param thresh detection threshold as a proportion of the peak of the 
#' infusion profile
#' @return Numeric vector of detected infusion scans.
#' @seealso \code{\link{detectParameters}}
#' @examples 
#' if (requireNamespace("metaboData", quietly = TRUE)) {
#'    detectInfusionScans(
#'       metaboData::filePaths('FIE-HRMS',
#'                             'BdistachyonEcotypes')[1])
#' }
#' @importFrom mzR openMSfile header
#' @importFrom dplyr group_by summarise
#' @export

detectInfusionScans <- function(files, 
																thresh = 0.5){
	
	ms <- files %>%
		future_map(~{
			.x %>%
				openMSfile() %>%
				header()
		}) %>%
		set_names(files)
	
	hd <- ms %>%
		bind_rows(.id = 'Sample') %>%
		as_tibble() %>%
		select(Sample,seqNum,acquisitionNum,polarity,totIonCurrent,filterString) %>%
		split(.$polarity) %>%
		map(~{
			d <- .
			d %>%
				split(.$Sample) %>%
				map(~{
					a <- .
					a %>%
						split(.$filterString) %>%
						map(~{
							b <- .
							b %>%
								mutate(acquisitionNum = seq_len(nrow(.)))
						}) %>%
						bind_rows()
				}) %>%
				bind_rows() %>%
				select(Sample,acquisitionNum,polarity,totIonCurrent,filterString)
		}) %>%
		bind_rows() %>%
		group_by(acquisitionNum) %>%
		summarise(totIonCurrent = mean(totIonCurrent))
	
	mTIC <- hd$totIonCurrent %>%
		max()
	
	TICthresh <- mTIC * thresh
	scans <- hd$acquisitionNum[hd$totIonCurrent > TICthresh]
	return(min(scans):max(scans))
}

#' Detect suitable spectral binning parameters
#' @description Detect binning parameters from a given list of file paths.
#' @param files character vector of file paths
#' @return S4 object of class BinParameters
#' @examples 
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' parameters <- detectParameters(files[1])
#' @seealso \code{\link{BinParameters-class}}, \code{\link{binParameters}}
#' @export

detectParameters <- function(files){
	
	scans <- detectInfusionScans(files)
	
	bp <- binParameters(scans = scans)
	return(bp)
}

