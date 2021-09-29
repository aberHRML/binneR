
test_that("setting BINNER_DP works", {
	Sys.setenv(BINNER_DP = 3)
	
	expect_equal(binnerDPenv(),3)
	
	Sys.unsetenv('BINNER_DP')
})

test_that("warning thrown when BINNER_DP set above 5", {
	Sys.setenv(BINNER_DP = 6)
	
	expect_warning(binnerDPenv())
	
	Sys.unsetenv('BINNER_DP')
})

test_that("warning thrown when BINNER_DP set as non-numeric", {
	Sys.setenv(BINNER_DP = 'a')
	
	expect_warning(binnerDPenv())
	
	Sys.unsetenv('BINNER_DP')
})

test_that("setting binner_dp works", {
	options(binner_dp = 3)
	
	expect_equal(binnerDPopt(),3)
	
	options(binner_dp = NULL)
})

test_that("warning thrown when binner_dp set above 5", {
	options(binner_dp = 6)
	
	expect_warning(binnerDPopt())
	
	options(binner_dp = NULL)
})

test_that("warning thrown when binner_dp set as non-numeric", {
	options(binner_dp = 'a')
	
	expect_warning(binnerDPopt())
	
	options(binner_dp = NULL)
})

test_that("warning thrown when BINNER_DP and binner_dp are differentially set", {
	options(binner_dp = 3)
	Sys.setenv(BINNER_DP = 4)
	
	expect_warning(binnerDP())
	
	options(binner_dp = NULL)
	Sys.unsetenv('BINNER_DP')
})
