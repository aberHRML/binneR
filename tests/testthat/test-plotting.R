
context('measures plots')

file <- system.file('example-data/1.mzML.gz',package = 'binneR')

info <- tibble::tibble(fileOrder = 1,
											 injOrder = 1,
											 fileName = basename(file),
											 batch = 1,
											 block = 1,
											 name = '1',
											 class = 1)

p <- binParameters(scans = 5:13,cls = 'class')
p_no_cls <- binParameters(scans = 5:13)

analysis <- binneRlyse(file, 
											 info, 
											 parameters = p,verbose = FALSE)

analysis_no_cls <- binneRlyse(file, 
											 info, 
											 parameters = p_no_cls,verbose = FALSE)

test_that('plotChromFromFile works',{
	pl <- plotChromFromFile(file,scans = c(5,13))
	expect_s3_class(pl,'ggplot')
})

test_that('plotChromatogram works',{
	pl <- plotChromatogram(analysis)
	expect_s3_class(pl,'ggplot')
})

test_that('plotTIC works',{
	pl <- plotTIC(analysis)
	expect_s3_class(pl,'ggplot')
})

test_that('plotFingerprint works',{
	pl <- plotFingerprint(analysis)
	expect_s3_class(pl,'ggplot')
})

test_that('plotBin works for all',{
	pl <- plotBin(analysis,'n133.01',type = 'all')
	expect_s3_class(pl,"ggplot")
})

test_that('plotBin works for cls',{
	pl <- plotBin(analysis,'n133.01',type = 'cls')
	expect_s3_class(pl,"ggplot")
})

test_that('plotBin errors with no cls available',{
	expect_error(plotBin(analysis_no_cls,'n133.01',type = 'cls'))
})

test_that('plotBin works for sample',{
	pl <- plotBin(analysis,'n133.01',type = 'sample')
	expect_s3_class(pl,"ggplot")
})

test_that('plotBin errors with incorrect bin',{
	expect_error(plotBin(analysis,bin = 'wrong'))
})

test_that('plotPurity works',{
	pl <- plotPurity(analysis)
	expect_s3_class(pl,"ggplot")
})

test_that('plotCentrality works',{
	pl <- plotCentrality(analysis)
	expect_s3_class(pl,"ggplot")
})
