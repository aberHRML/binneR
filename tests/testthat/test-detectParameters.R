library(metaboData)

context('detectParameters')

test_that('detectParameters works',{
	files <- system.file('example-data/1.mzML.gz',package = 'binneR')
	
	parameters <- detectParameters(files[1])
	
	expect_true(class(parameters) == 'BinParameters')
})
