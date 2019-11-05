test_that("create technology intensity dataframe", {

    ## loading the test data of the year 2002 WIOD for exchanges
    ## between FRA and USA
    yearly.mini.raw <- readRDS("./usa_fra_w2002.rds")

    ## testing for each isic code
    tech.int.0 <- getTechIntensity(yearly.mini.raw, 0)
    tech.int.1 <- getTechIntensity(yearly.mini.raw, 1)
    tech.int.2 <- getTechIntensity(yearly.mini.raw, 2)

    expect_equal(dim(tech.int.0), c(69,2))
    expect_equal(names(tech.int.0), c("RNr", "NewIndustryCode"))
    
    expect_equal(dim(tech.int.1), c(69,2))
    expect_equal(names(tech.int.1), c("RNr", "NewIndustryCode"))

    expect_equal(dim(tech.int.2), c(69,2))
    expect_equal(names(tech.int.2), c("RNr", "NewIndustryCode"))

})
