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
	
	dp <- binnerDP()
		
	pks <- future_map(files,getFile,scans = scans) %>%
		set_names(files) %>%
		bind_rows(.id = 'fileName') %>%
		mutate(fileName = basename(fileName),
									mz = round(mz,5),bin = round(mz,dp))
	return(pks)
}

#' @importFrom mzR header

getHeaders <- function(files){
	
	available_header_temps <- availableHeaderTemps(files)
	
	file_headers <- available_header_temps %>% 
		future_map(readRDS)
	
	file_names <- files %>% 
		basename() %>%
		file_path_sans_ext(compression = TRUE)
		
	temp_file_names <- available_header_temps %>% 
		basename() %>% 
		file_path_sans_ext()
	
	unavailable_headers <- files[!(file_names %in% temp_file_names)]
	
	if (length(unavailable_headers > 0)){
		headers <- files %>% 
			future_map(~{
				ms <- .x %>%
					openMSfile(backend = 'pwiz') 
				
				file_header <- ms %>%
					header()
				
				return(file_header)
			})	
		
		file_headers <- c(file_headers,headers)
	}
	
	file_headers <- file_headers %>%
		set_names(files) %>%
		bind_rows(.id = 'FileName') %>%
		select(FileName,acquisitionNum,totIonCurrent,polarity,filterString)
	
	return(file_headers)
}

