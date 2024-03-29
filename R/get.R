#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom magrittr %>%

sampProcess <- function(file,scans,dp){
	
	pl <- getFile(file,scans) %>%
		mutate(mz = round(mz,dp)) %>% 
		group_by(polarity,mz) %>% 
		summarise(intensity = sum(intensity)/length(scans)) 
	
	return(pl)
}

#' @importFrom mzR openMSfile peaks close
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom magrittr set_colnames set_names

getFile <- function(file,scans){
	
	ms <- openMSfile(file,backend = 'pwiz')
	
	hd <- header(ms)
	
	headerTemp(file,hd)
	
	hd <- hd %>%
		select(seqNum,polarity,filterString) %>%
		group_by(polarity,filterString) %>%
		mutate(scan = 1:dplyr::n()) %>%
		filter(scan %in% scans) 
	
	hd$polarity[hd$polarity == 0] <- 'n'
	hd$polarity[hd$polarity == 1] <- 'p'
	
	file_peaks <- ms %>%
		peaks() %>%
		.[hd$seqNum] %>%
		map(~{
			d <- .
			d %>%
				set_colnames(c('mz','intensity')) %>%
				as_tibble() %>%
				filter(intensity > 0)
		}) %>%
		set_names(hd$seqNum) %>%
		bind_rows(.id = 'seqNum') %>%
		mutate(seqNum = as.numeric(seqNum)) %>%
		left_join(hd, by = "seqNum") %>%
		select(-filterString,-seqNum)
	
	return(file_peaks)
}

#' @importFrom parallel makeCluster stopCluster parLapply clusterExport
#' @importFrom dplyr mutate

getPeaks <- function(files,scans){
	
	idx <- tibble(
		fileName = files
	) %>% 
		rowid_to_column(var = 'idx')
	
	dp <- binnerDP()
		
	pks <- future_map(files,getFile,scans = scans) %>%
		set_names(idx$idx) %>% 
		bind_rows(.id = 'idx') %>%
		mutate(idx = as.numeric(idx)) %>% 
		left_join(idx,
												by = 'idx') %>% 
		mutate(fileName = basename(fileName),
									mz = round(mz,5),bin = round(mz,dp))
	return(pks)
}

#' @importFrom mzR header

getHeaders <- function(files){
	
	idx <- tibble(
		FileName = files
	) %>% 
		rowid_to_column(var = 'idx')
	
	available_header_temps <- availableHeaderTemps(files)
	
	file_headers <- available_header_temps %>% 
		future_map(readRDS)
	
	file_headers <- file_headers %>%
		set_names(idx$idx) %>% 
		bind_rows(.id = 'idx') %>%
		mutate(
			idx = as.numeric(idx)
		) %>% 
		left_join(idx,
												by = 'idx') %>% 
		select(idx,FileName,acquisitionNum,totIonCurrent,polarity,filterString) %>% 
		as_tibble()
	
	return(file_headers)
}

