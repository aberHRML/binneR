
context('detectInfusionScans')

test_that('detectInfusionScans works',{
	file <- metaboData::filePaths('FIE-HRMS',
																															'BdistachyonTechnical',
																															ask = FALSE)[1]
	
	scans <- detectInfusionScans(file)
	
	expect_true(class(scans) == 'integer')
	expect_true(identical(scans,as.integer(c(5:13))))
})
