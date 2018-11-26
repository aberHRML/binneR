#' @importFrom utils install.packages

.onLoad <- function(libname,pkgname) {
	if (!requireNamespace("metaboData", quietly = TRUE)) {
		
		if (Sys.info()['sysname'] != 'Linux') {
			type <- 'both'
		} else {
			type <- 'source'
		}
		
		install.packages("metaboData",repos = c('https://cloud.r-project.org',"https://aberhrml.github.io/drat/"),type = type)
	}
}