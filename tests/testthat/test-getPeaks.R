
context('getPeaks')

file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]

test_that('sampProcess works',{
	pks <- sampProcess(file,5:13,2)
	
	expect_s3_class(pks,'tbl_df')
	expect_equal(ncol(pks),3)
})

test_that('getPeaks works single core',{
	pks <- getPeaks(file,5:13,0,detectClusterType())
	
	expect_s3_class(pks,'tbl_df')
	expect_equal(ncol(pks),6)
})

test_that('getHeaders works single core',{
	h <- getHeaders(file,0,detectClusterType())
	expect_s3_class(h,'data.frame')
})