test_that("the technology intensity is merged with main df", {

    ## loading the test data of the year 2002 WIOD for exchanges
    ## between FRA and USA
    yearly.mini.raw <- get(load("./w2002.rda"))
    rnr.ind.test <- get(load("./rnr_industry.rda"))

    list.tech.int <- addTechIntensity(yearly.mini.raw, 2)

    added.mini.raw <- list.tech.int[[1]]
    industry.RNr <- list.tech.int[[2]]
            
    ## test dimension of data    
    expect_equal(dim(added.mini.raw), c(120, 128))
    expect_equal(dim(industry.RNr), c(69,2))

    ## counting manufacturing with RNr from rnr.industry
    row.count.func <- added.mini.raw %>%
        filter(RNr %in% rnr.ind.test$RNr[grepl("Tech", rnr.ind.test$NewIndustryCode)==TRUE] &
               Country=="FRA") %>%
        select(Country, RNr, IndustryCode) %>% nrow

    ## counting manufacturing with Industry code from add.mini.raw
    row.count.out <- added.mini.raw %>%
        filter(grepl("Tech",IndustryCode) & Country=="FRA") %>%
        select(Country, RNr, IndustryCode) %>% nrow        
    
    # test RNr and IndustryCode
    expect_equal(row.count.func, row.count.out)
})



