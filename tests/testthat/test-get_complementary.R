test_that("complementary matrix", {

    ## complementary data
    w2002.comp <- get(load("./wide_comp_w2002.rda"))

    industry.RNr <- get(load("./rnr_industry.rda"))
    
    va.df <- getComplementary(w2002.comp, industry.RNr, "VA")
    expadj.df <- getComplementary(w2002.comp, industry.RNr, "EXP_adj")

    expect_equal(dim(va.df), c(85, 2))
    expect_equal(dim(expadj.df), c(85, 2))
    
})
