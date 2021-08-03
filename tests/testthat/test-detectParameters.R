library(metaboData)

context('detectParameters')

test_that('detectParameters works',{
	files <- metaboData::filePaths('FIE-HRMS',
																																'BdistachyonEcotypes',
																																ask = FALSE)
	
	parameters <- detectParameters(files[1])
	
	expect_true(class(parameters) == 'BinParameters')
})
