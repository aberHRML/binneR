
context('readBinningParameters')

test_that('readBinningParameters works',{
	file <- system.file('binning_parameters.yml',package = 'binneR')
	parameters <- readBinningParameters(file)
	
	expect_s4_class(parameters,'BinParameters')
})

test_that('readBinningParameters errors with incorrect scan range format',{
	temp_dir <- tempdir()
	temp_file <- paste(temp_dir,'binning_parameters.yml',sep = '/')
	writeLines('scans: wrong',temp_file)
	
	expect_error(readBinningParameters(temp_file))
})
