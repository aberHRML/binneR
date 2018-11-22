
.onLoad <- function(libname,pkgname) {
	if (!requireNamespace("metaboData", quietly = TRUE)) {
		install.packages("metaboData",repos = "https://aberhrml.github.io/drat/")
	}
}