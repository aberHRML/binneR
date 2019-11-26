
context('measures plots')

file <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1]

info <- tibble::tibble(fileOrder = 1,
											 injOrder = 1,
											 fileName = basename(file),
											 batch = 1,
											 block = 1,
											 name = '1',
											 class = 1)

p <- binParameters(scans = 5:13,nCores = 2,clusterType = detectClusterType())

pars <- list(scans = scans(p),nCores = nCores(p),clusterType = clusterType(p))

analysis <- binneRlyse(file, 
											 info, 
											 parameters = p,verbose = F)

test_that('plotPurity works',{
	purPlot <- plotPurity(analysis)
	expect_identical(class(purPlot),c("gg","ggplot"))
})

test_that('plotCentrality works',{
	centPlot <- plotCentrality(analysis)
	expect_identical(class(centPlot),c("gg","ggplot"))
})