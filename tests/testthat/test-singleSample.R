
context('singleSample')

test_that('single sample works',{
	file <- system.file('example-data/1.mzML.gz',package = 'binneR')
	bd <- singleSample(file,class = 'test',verbose = TRUE)
	
	expect_s4_class(bd,'Binalysis')
})

test_that('single sample works with no class specified',{
	file <- system.file('example-data/1.mzML.gz',package = 'binneR')
	bd <- singleSample(file,verbose = TRUE)
	
	expect_s4_class(bd,'Binalysis')
})

test_that('single samples errors with > 1 file',{
	file <- system.file('example-data/1.mzML.gz',package = 'binneR')
	file <- rep(file,2)
	
	expect_error(singleSample(file,verbose = FALSE))
})

test_that('single samples errors with > 1 class',{
	file <- system.file('example-data/1.mzML.gz',package = 'binneR')
	
	expect_error(singleSample(file,class = c('wrong','incorrect'),verbose = FALSE))
})
