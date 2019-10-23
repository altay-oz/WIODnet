test_that("complementary matrix", {

    ## complementary data
    w2002.comp <<- get(load("./wide_comp_w2002.rda"))
    
    va.df <- getComplementary(w2002.comp, "VA")
    expadj.df <- getComplementary(w2002.comp, "EXP_adj")

    
    expect_equal(dim(va.df), c(85, 2))
    expect_equal(dim(expadj.df), c(85, 2))
    
})
