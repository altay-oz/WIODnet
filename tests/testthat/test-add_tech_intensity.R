test_that("the technology intensity is merged with main df", {

    ## loading the test data of the year 2002 WIOD for exchanges
    ## between FRA and USA
    yearly.mini.raw <- get(load("./w2002.rda"))
    rnr.industry <- get(load("./rnr_industry.rda"))

    added.mini.raw <- addTechIntensity(yearly.mini.raw)

    # test dimension of data    
    expect_equal(dim(added.mini.raw), c(120, 128))
    
    # test RNr and IndustryCode
    expect_equal(
        # counting manufacturing with RNr from rnr.industry
        added.mini.raw %>% filter(RNr %in% rnr.industry$RNr[grepl("Tech",rnr.industry$NewIndustryCode)==TRUE] & Country=="FRA") %>%  select(Country, RNr, IndustryCode) %>% nrow,    
        # counting manufacturing with Industry code from add.mini.raw
        added.mini.raw %>% filter(grepl("Tech",IndustryCode) & Country=="FRA") %>% select(Country, RNr, IndustryCode) %>% nrow        
    )
})



