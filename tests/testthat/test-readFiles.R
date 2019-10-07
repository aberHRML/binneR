
context('readFiles')

test_that('readFiles works',{
	file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]
	
	res <- readFiles(file,dp = 2,scans = 5:13)

	expect_true(class(res) == 'list')
	expect_true(length(res) == 2)
	expect_true(identical(names(res),c('n','p')))
	expect_true(identical(purrr::map_dbl(res,ncol),c(n = 1739,p = 1942)))
	expect_true(identical(purrr::map_dbl(res,nrow),c(n = 1,p = 1)))
})
