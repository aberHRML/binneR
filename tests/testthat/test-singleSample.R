
context('singleSample')

test_that('single sample works',{
	file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]
	bd <- singleSample(file,class = 'test',verbose = TRUE)
	
	expect_s4_class(bd,'Binalysis')
})

test_that('single samples errors with > 1 file',{
	file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1:2]
	
	expect_error(singleSample(file,verbose = FALSE))
})

test_that('single samples errors with > 1 file',{
	file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1:2]
	
	expect_error(singleSample(file,verbose = FALSE))
})

test_that('single samples errors with > 1 class',{
	file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]
	
	expect_error(singleSample(file,class = c('wrong','incorrect'),verbose = FALSE))
})
