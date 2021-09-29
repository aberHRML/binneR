
context('readFiles')

test_that('readFiles works',{
	file <- system.file('example-data/1.mzML.gz',package = 'binneR')
	
	res <- readFiles(file,dp = 2,scans = 5:13)

	expect_type(res,'list')
	expect_equal(length(res),2)
	expect_identical(names(res),c('n','p'))
	expect_identical(purrr::map_dbl(res,ncol),c(n = 1739,p = 1942))
	expect_identical(purrr::map_dbl(res,nrow),c(n = 1,p = 1))
})
