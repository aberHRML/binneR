library(metaboData)

context('detectParameters')

test_that('detectParameters works',{
	files <- filePaths('FIE-HRMS','BdistachyonEcotypes')
	
	parameters <- detectParameters(files[1])
	
	expect_true(class(parameters) == 'BinParameters')
})
