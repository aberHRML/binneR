#' detectInfusionScans
#' @description detect infusion scans for a set of FIE-MS infusion profiles.
#' @param files character vector of file paths to use
#' @param sranges A list of vectors containing the scan events present.
#' @param thresh detection threshold as a proportion of  preak of the infusion profile
#' @importFrom mzR openMSfile header
#' @importFrom dplyr group_by summarise
#' @examples 
#' if (requireNamespace("metaboData", quietly = TRUE)) {
#'    detectInfusionScans(metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1])
#' }
#' @export

detectInfusionScans <- function(files, sranges = list(c(70,1000)), thresh = 0.5){
	ms <- files %>%
		map(~{
			d <- .
			d %>%
				openMSfile() %>%
				header()
		})
	names(ms) <- files
	
	hd <- ms %>%
		bind_rows(.id = 'Sample') %>%
		as_tibble() %>%
		select(Sample,seqNum,acquisitionNum,polarity,totIonCurrent) %>%
		split(.$polarity) %>%
		map(~{
			d <- .
			d %>%
				split(.$Sample) %>%
				map(~{
					a <- .
					a %>%
						split(rep(1:length(sranges),nrow(.)/length(sranges))) %>%
						map(~{
							b <- .
							b %>%
								mutate(acquisitionNum = 1:nrow(.))
						}) %>%
						bind_rows()
				}) %>%
				bind_rows() %>%
				select(Sample,acquisitionNum,totIonCurrent)
		}) %>%
		bind_rows(.id = 'Polarity') %>%
		group_by(acquisitionNum) %>%
		summarise(totIonCurrent = mean(totIonCurrent))
	mTIC <- hd$totIonCurrent %>%
		max()
	
	TICthresh <- mTIC * thresh
	scans <- hd$acquisitionNum[hd$totIonCurrent > TICthresh]
	c(min(scans),max(scans))
}