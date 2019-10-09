test_that("create technology intensity dataframe", {
    ## loading the test data of the year 2002 WIOD for exchanges
    ## between FRA and USA
    yearly.raw <- get(load("./w2002.rda"))
    outdat <- getTechIntensity(yearly.raw)

    expect_equal(dim(outdat), c(61,2))
    expect_equal(names(outdat), c("RNr", "NewIndustryCode"))
})
