test_that("complementary matrix", {

    ## complementary data
    w2002.comp <- readRDS("./wide_comp_w2002.rds")

    industry.RNr <- readRDS("./rnr_industry.rds")
    
    va.df <- getComplementary(w2002.comp, industry.RNr, "VA")
    expadj.df <- getComplementary(w2002.comp, industry.RNr, "EXP_adj")

    expect_equal(dim(va.df), c(85, 2))
    expect_equal(dim(expadj.df), c(85, 2))
    
})
