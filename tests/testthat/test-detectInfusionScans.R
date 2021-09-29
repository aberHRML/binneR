
context('detectInfusionScans')

test_that('detectInfusionScans works',{
	file <- system.file('example-data/1.mzML.gz',package = 'binneR')
	
	scans <- detectInfusionScans(file)
	
	expect_type(scans,'integer')
	expect_identical(scans,as.integer(c(5:13)))
})
