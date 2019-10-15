test_that("obtain long network table", {

    ## network matrix
    w2002.IO <<- get(load("./wide_IO_w2002.rda"))
    
    mini.long <- getNetLong(w2002.IO)
    
    ## check dimension of data
    expect_equal(dim(mini.long), c(6888, 3))

    ## check list of nodes (country-industry)
    expect_equal(
        w2002.IO %>% mutate(ID=paste(Country,IndustryCode,sep=".")) %>% select(ID) %>% unique %>% nrow,
        combine(unique(mini.long$source), unique(mini.long$target)) %>% data.frame %>% filter(substr(.,5,5)!="Z")  %>% unique  %>% nrow)
})
