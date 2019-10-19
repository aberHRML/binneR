
context('singleSample')

test_that('single sample works',{
	file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]
	bd <- singleSample(file,nCores = 1,verbose = F)
	
	expect_true(class(bd) == 'Binalysis')
})