
context('readFiles')

test_that('readFiles works',{
	file <- metaboData::filePaths('FIE-HRMS',
																															'BdistachyonTechnical',
																															ask = FALSE)[1]
	
	res <- readFiles(file,dp = 2,scans = 5:13)

	expect_type(res,'list')
	expect_equal(length(res),2)
	expect_identical(names(res),c('n','p'))
	expect_identical(purrr::map_dbl(res,ncol),c(n = 1660,p = 2985))
	expect_identical(purrr::map_dbl(res,nrow),c(n = 1,p = 1))
})
