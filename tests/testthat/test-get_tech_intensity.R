test_that("create technology intensity dataframe", {

    ## loading the test data of the year 2002 WIOD for exchanges
    ## between FRA and USA
    yearly.mini.raw <- get(load("./w2002.rda"))

    tech.int <- getTechIntensity(yearly.mini.raw)

    expect_equal(dim(tech.int), c(69,2))
    expect_equal(names(tech.int), c("RNr", "NewIndustryCode"))
    
})
