
context('readBinningParameters')

test_that('readBinningParameters works',{
	file <- system.file('binning_parameters.yml',package = 'binneR')
	parameters <- readBinningParameters(file)
	
	expect_s4_class(parameters,'BinParameters')
})