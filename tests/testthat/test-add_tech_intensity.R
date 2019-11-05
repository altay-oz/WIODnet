test_that("the technology intensity is merged with main df", {

    ## loading the test data of the year 2002 WIOD for exchanges
    ## between FRA and USA
    ## obtained from original file
    ## yearly.raw <- get(load("./wiod_original_data/WIOT2002_October16_ROW.RData"))

    ## usa.fra.2002 <- yearly.raw %>% filter(Country %in% c("FRA", "USA", "TOT")) %>%
    ##     select("IndustryCode", "IndustryDescription", "Country", "RNr", "Year",
    ##            starts_with("FRA"), starts_with("USA"), "TOT")

    ## saveRDS(usa.fra.2002, "usa_fra_w2002.rds")

    yearly.mini.raw <- readRDS("./usa_fra_w2002.rds")

    list.tech.int <- addTechIntensity(yearly.mini.raw, 2)

    added.mini.raw <- list.tech.int[[1]]
    industry.RNr <- list.tech.int[[2]]
    
    ## test dimension of data    
    expect_equal(dim(added.mini.raw), c(120, 128))
    expect_equal(dim(industry.RNr), c(69,2))

    ## counting manufacturing with RNr from rnr.industry
    row.count.func <- added.mini.raw %>%
        filter(RNr %in% industry.RNr$RNr[grepl("Tech", industry.RNr$NewIndustryCode)==TRUE] &
               Country=="FRA") %>%
        select(Country, RNr, IndustryCode) %>% nrow

    ## counting manufacturing with Industry code from add.mini.raw
    row.count.out <- added.mini.raw %>%
        filter(grepl("Tech",IndustryCode) & Country=="FRA") %>%
        select(Country, RNr, IndustryCode) %>% nrow        
    
    # test RNr and IndustryCode
    expect_equal(row.count.func, row.count.out)
})
