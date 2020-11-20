
context('singleSample')

test_that('single sample works',{
	file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]
	bd <- singleSample(file,nCores = 1,verbose = TRUE)
	
	expect_true(class(bd) == 'Binalysis')
})