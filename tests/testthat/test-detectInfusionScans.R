
context('detectInfusionScans')

test_that('detectInfusionScans works',{
	file <- metaboData::filePaths('FIE-HRMS',
																															'BdistachyonTechnical',
																															ask = FALSE)[1]
	
	scans <- detectInfusionScans(file)
	
	expect_type(scans,'integer')
	expect_identical(scans,as.integer(c(5:11)))
})
