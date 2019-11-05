test_that("dividing the raw data into two df", {

    ## loading the test data of the year 2002 WIOD for exchanges
    ## between FRA and USA
    yearly.mini.raw <- readRDS("./usa_fra_w2002.rds")
    
    outdat <- divideRawData(yearly.mini.raw)

    expect_equal(length(outdat), 2)
    expect_equal(dim(outdat[[1]]), c(112,128))

})
