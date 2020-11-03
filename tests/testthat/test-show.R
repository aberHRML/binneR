
context('show methods')

test_that('BinParameters class show method works',{
	bp <- new('BinParameters')
	expect_output(print(bp),'Scans:')
})

test_that('Binalysis class show method works',{
	ba <- new('Binalysis')
	expect_output(print(ba),'Samples:')
})