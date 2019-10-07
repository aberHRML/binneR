
context('plotChromFromFile')

test_that('plotChromFromFile works',{
	pl <- plotChromFromFile(metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1],scans = c(5,13))
	
	expect_true(identical(class(pl),c('gg','ggplot')))
})
