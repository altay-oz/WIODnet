test_that("create technology intensity dataframe", {

    tech.int <<- getTechIntensity(yearly.mini.raw)

    expect_equal(dim(tech.int), c(69,2))
    expect_equal(names(tech.int), c("RNr", "NewIndustryCode"))
    
})
