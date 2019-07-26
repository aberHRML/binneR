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
	return(min(scans):max(scans))
}

#' detectSranges
#' @description Detect scan ranges from file list.
#' @param files character vector of file paths.
#' @importFrom magrittr set_names
#' @importFrom stringr str_split str_locate str_extract str_split_fixed
#' @importFrom purrr map_dbl
#' @export

detectSranges <- function(files){
	
	scanFilters <- files %>%
		map(openMSfile,backend = 'pwiz') %>%
		map(~{
			header(.) %>%
				select(filterString) %>%
				distinct()
			}) %>%
		bind_rows() %>%
		distinct() %>%
		unlist()
	
	modePosition <- scanFilters[1] %>%
			{str_locate(.,pattern = 'p ESI Full ms')[1,1] - 2}
		
	
	modes <- map_chr(scanFilters,str_sub,start = modePosition,end = modePosition)
	
	f <- scanFilters %>% 
		split(modes) %>%
		map(~{
			map_chr(.,str_extract,pattern = '\\[(.*?)\\]') %>%
				str_replace_all('\\[','') %>%
				str_replace_all('\\]','') %>%
				str_split_fixed('-',2) %>%
				{suppressWarnings(as_tibble(.))} %>%
				rename(low = V1,
							 high = V2) %>%
				mutate(low = as.numeric(low),
							 high = as.numeric(high)) %>%
				rowid_to_column(var = 'event')
		}) %>%
		bind_rows(.id = 'mode')
	
	sranges <- f %>%
		split(.$event) %>%
		map(~{
			c(min(.$low),max(.$high))
		}) %>%
		unname()
	
	return(sranges)
}

#' detectModes
#' @description Detect ionisation modes from file list.
#' @param files character vector of file paths
#' @export

detectModes <- function(files){
	ms <- openMSfile(files[1],backend = 'pwiz') %>%
		header() %>%
		.$polarity %>%
		unique()
	
	ms[ms == '0'] <- 'n'
	ms[ms == '1'] <- 'p'
	return(ms)
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
#' @export

detectParameters <- function(files){
	
	sranges <- detectSranges(files)
	scans <- detectInfusionScans(files,sranges = sranges)
	modes <- detectModes(files)
	nCores <- detectCores() * 0.75
	clusterType <- detectClusterType()
	
	bp <- binParameters(scans,modes,sranges,nCores = nCores,clusterType = clusterType)
	return(bp)
}

