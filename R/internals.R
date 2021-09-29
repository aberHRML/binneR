
binnerDPenv <- function(){
	dp <- Sys.getenv('BINNER_DP')
	
	if (dp != '') {
		dp <- suppressWarnings(as.numeric(dp))
	} else {
		dp <- 2
	}
	
	if (is.na(dp)){
		warning("The environment variable 'BINNER_DP' is not numeric. Using 2 decimal places instead.",
										call. = FALSE)
		dp <- 2
	}
	
	if (dp > 5){
		warning("The environment variable 'BINNER_DP' is greater than 5. Using 2 decimal places instead.",
										call. = FALSE)
		dp <- 2
	}
	
	return(dp)
}

binnerDPopt <- function(){
	dp <- options()$binner_dp
	
	if (is.null(dp)){
		dp <- 2
	}
	
	if (!is.numeric(dp)){
		warning("The global option 'binner_dp' is not numeric. Using 2 decimal places instead.",
										call. = FALSE)
		dp <- 2
	}
	
	if (dp > 5){
		warning("The global option 'binner_dp' is greater than 5. Using 2 decimal places instead.",
										call. = FALSE)
		dp <- 2
	}
	
	return(dp)
}

binnerDP <- function(){
	
	dp_env <- binnerDPenv()
	
	dp_opt <- binnerDPopt()
	
	dp <- c(dp_opt,
									dp_env) %>% 
		unique()
		
		if (length(dp) > 1) {
			warning("The environment variable 'BINNER_DP' and global option 'binner_dp' are differentially set. Using the value of 'binner_dp'.",
											call. = FALSE)
			
			dp <- dp[1]
		}
	
	return(dp)
}

#' @importFrom tools file_path_sans_ext

headerTemp <- function(file,header_table){
	temp_dir <- paste0(tempdir(),'/binneR-headers')
	
	if (!dir.exists(temp_dir)){
		dir.create(temp_dir)
	}
	
	file_name <- file %>% 
		basename() %>% 
		file_path_sans_ext(compression = TRUE)
	
	temp_file <- paste0(temp_dir,'/',file_name,'.rds')
	saveRDS(header_table,temp_file)
}

availableHeaderTemps <- function(files){
	temp_dir <- paste0(tempdir(),'/binneR-headers')
	
	temp_files <- files %>% 
		basename() %>% 
		file_path_sans_ext(compression = TRUE) %>% 
		{paste0(temp_dir,'/',.,'.rds')}
	
	temp_files <- temp_files[file.exists(temp_files)]
	
	return(temp_files)
}
