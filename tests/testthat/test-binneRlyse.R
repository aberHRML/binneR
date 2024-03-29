
context('binneRlyse')

file <- system.file('example-data/1.mzML.gz',package = 'binneR')

info <- tibble::tibble(fileOrder = 1,
											 injOrder = 1,
											 fileName = basename(file),
											 batch = 1,
											 block = 1,
											 name = '1',
											 class = 1)

p <- binParameters(scans = 5:13,cls = 'class')

pars <- list(scans = scans(p))

analysis <- binneRlyse(file, 
											 info, 
											 parameters = p,verbose = TRUE)

inf <- sampleInfo(analysis)
bd <- binnedData(analysis)
ad <- accurateData(analysis)

test_that('binParameters works',{
	expect_true(class(p) == 'BinParameters')
	expect_true(identical(slotNames(p),c("scans","cls")))
	expect_true(identical(pars$scans,5:13))
})

test_that('binneRlyse works',{
	expect_true(class(analysis) == 'Binalysis')
	
	expect_true(identical(class(inf),c('tbl_df','tbl','data.frame')))
	expect_true(nrow(inf) == 1)
	expect_true(ncol(inf) == 7)
	
	expect_true(class(bd) == 'list')
	expect_true(identical(names(bd),c('n','p')))
	expect_identical(purrr::map_dbl(bd,nrow),c(n = 1,p = 1))
	expect_identical(purrr::map_dbl(bd,ncol),c(n = 854,p = 1042))
	
	expect_identical(class(ad),c('tbl_df','tbl','data.frame'))
	expect_equal(nrow(ad),1896)
	expect_equal(ncol(ad),9)
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
	
	expect_equal(scans(bp),1)
	expect_equal(cls(bp),'class')
})

test_that('Binning parameters can be returned from Binalysis class',{
	bp <- binningParameters(analysis)
	expect_s4_class(bp,'BinParameters')
})
