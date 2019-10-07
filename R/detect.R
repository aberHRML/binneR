#' detectInfusionScans
#' @description detect infusion scans for a set of FIE-MS infusion profiles.
#' @param files character vector of file paths to use
#' @param sranges A list of vectors containing the scan events present.
#' @param thresh detection threshold as a proportion of  preak of the infusion profile
#' @param nCores the number of cores to use for parallel processing
#' @param clusterType the type of cluster to use for parallel processing
#' @importFrom mzR openMSfile header
#' @importFrom dplyr group_by summarise
#' @examples 
#' if (requireNamespace("metaboData", quietly = TRUE)) {
#'    detectInfusionScans(metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1])
#' }
#' @export

detectInfusionScans <- function(files, sranges = list(c(70,1000)), thresh = 0.5, nCores = detectCores() * 0.75, clusterType = detectClusterType()){
	
	nSlaves <- ceiling(length(files) / 10)
	
	if (nSlaves > nCores) {
		nSlaves <- nCores	
	}
	
	clus <- makeCluster(nSlaves,clusterType)
	
	ms <- files %>%
		parLapply(clus,.,function(d){
			d %>%
				openMSfile() %>%
				header()
		}) %>%
		set_names(files)
	
	stopCluster(clus)
	
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
	return(min(scans):max(scans))
}

#' detectClusterType
#' @description Detect appropriate cluster type from OS.
#' @export

detectClusterType <- function(){
	if (.Platform$OS.type == 'windows') {
		type <- 'PSOCK'
	} else {
		type <- 'FORK'
	}
	return(type)
}

#' detectParameters
#' @description Detect binning parameters from file list.
#' @param files character vector of file paths
#' @param nCores the number of cores to use for parallel processing
#' @param clusterType the type of cluster to use for parallel processing
#' @examples 
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' parameters <- detectParameters(files[1])
#' @export

detectParameters <- function(files, nCores = detectCores() * 0.75, clusterType = detectClusterType()){
	
	scans <- detectInfusionScans(files,sranges = sranges,nCores = nCores,clusterType = clusterType)
	
	bp <- binParameters(scans = scans,nCores = nCores,clusterType = clusterType)
	return(bp)
}

