
context('binneRlyse')

file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]

info <- tibble::tibble(fileOrder = 1,
											 injOrder = 1,
											 fileName = basename(file),
											 batch = 1,
											 block = 1,
											 name = '1',
											 class = 1)

p <- binParameters(scans = 5:13,cls = 'class',nCores = 2,clusterType = detectClusterType())

pars <- list(scans = scans(p),nCores = nCores(p),clusterType = clusterType(p))

analysis <- binneRlyse(file, 
											 info, 
											 parameters = p,verbose = TRUE)

inf <- info(analysis)
bd <- binnedData(analysis)
ad <- accurateData(analysis)

chrPl <- plotChromatogram(analysis)
ticPl <- plotTIC(analysis)

binPl_all <- plotBin(analysis,'n133.01',type = 'all')
binPl_cls <- plotBin(analysis,'n133.01',type = 'cls')
binPl_sample <- plotBin(analysis,'n133.01',type = 'sample')

fingPl <- plotFingerprint(analysis)

test_that('binParameters works',{
	expect_true(class(p) == 'BinParameters')
	expect_true(identical(slotNames(p),c("scans","cls","nCores","clusterType")))
	expect_true(identical(pars$scans,5:13))
	expect_true(pars$nCores == 2)
	expect_true(pars$clusterType == detectClusterType())
})

test_that('binneRlyse works',{
	expect_true(class(analysis) == 'Binalysis')
	
	expect_true(identical(class(inf),c('tbl_df','tbl','data.frame')))
	expect_true(nrow(inf) == 1)
	expect_true(ncol(inf) == 7)
	
	expect_true(class(bd) == 'list')
	expect_true(identical(names(bd),c('n','p')))
	expect_true(identical(purrr::map_dbl(bd,nrow),c(n = 1,p = 1)))
	expect_true(identical(purrr::map_dbl(bd,ncol),c(n = 854,p = 1042)))
	
	expect_true(identical(class(ad),c('tbl_df','tbl','data.frame')))
	expect_true(nrow(ad) == 1896)
	expect_true(ncol(ad) == 8)
})

test_that('plots work',{
	expect_s3_class(chrPl,'ggplot')
	expect_s3_class(ticPl,'ggplot')
	
	expect_s3_class(binPl_all,'ggplot')
	expect_s3_class(binPl_cls,'ggplot')
	expect_s3_class(binPl_sample,'ggplot')
	
	expect_s3_class(fingPl,'ggplot')
})

test_that('BinParameters class show method works',{
	expect_output(print(p),'Scans:')
})

test_that('Binalysis class show method works',{
	expect_output(print(analysis),'Samples:')
})

test_that('binParameters can be correctly set',{
	bp <- new('BinParameters')
	scans(bp) <- 1
	cls(bp) <- 'class'
	nCores(bp) <- 1
	clusterType(bp) <- 'PSOCK'
	
	expect_equal(scans(bp),1)
	expect_equal(cls(bp),'class')
	expect_equal(nCores(bp),1)
	expect_equal(clusterType(bp),'PSOCK')
})
