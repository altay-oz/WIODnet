test_that("the technology intensity is merged with main df", {

    ## loading the test data of the year 2002 WIOD for exchanges
    ## between FRA and USA
    yearly.mini.raw <- get(load("./w2002.rda"))

    added.mini.raw <- addTechIntensity(yearly.mini.raw)

    expect_equal(dim(added.mini.raw), c(120, 128))

})
