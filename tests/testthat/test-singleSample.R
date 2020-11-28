
context('singleSample')

test_that('single sample works',{
	file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]
	bd <- singleSample(file,verbose = TRUE)
	
	expect_true(class(bd) == 'Binalysis')
})