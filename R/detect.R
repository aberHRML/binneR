#' Detect infusion profile scan range
#' @description Detect infusion scans for a set of FIE-MS infusion profiles.
#' @param files character vector of file paths to use
#' @param thresh detection threshold as a proportion of the peak of the 
#' infusion profile
#' @return Numeric vector of detected infusion scans.
#' @seealso \code{\link{detectParameters}}
#' @examples 
#' file_paths <- system.file('example-data/1.mzML.gz',package = 'binneR')
#' 
#' detectInfusionScans(file_paths)
#' @importFrom mzR openMSfile header
#' @importFrom dplyr group_by summarise
#' @export

detectInfusionScans <- function(files, 
																thresh = 0.5){
	
	idx <- tibble(
		Sample = files
	) %>% 
		rowid_to_column(var = 'idx')
	
	ms <- files %>%
		future_map(~{
			ms <- .x %>%
				openMSfile()
			
			file_header <- ms %>%
				header() %>% 
				as_tibble()
			
			return(file_header)
		}) %>%
		set_names(idx$idx) %>% 
		bind_rows(.id = 'idx') %>%
		mutate(
			idx = as.numeric(idx)
		) %>% 
		left_join(idx,
												by = 'idx',
												relationship = 'many-to-many')
	
	hd <-  ms %>%
		select(idx,seqNum,acquisitionNum,polarity,totIonCurrent,filterString) %>%
		group_by(idx,polarity,filterString) %>% 
		mutate(acquisitionNum = seq_len(n())) %>% 
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
#' file_paths <-system.file('example-data/1.mzML.gz',package = 'binneR')
#' parameters <- detectParameters(file_paths)
#' @seealso \code{\link{BinParameters-class}}, \code{\link{binParameters}}
#' @export

detectParameters <- function(files){
	
	scans <- detectInfusionScans(files)
	
	bp <- binParameters(scans = scans)
	return(bp)
}
