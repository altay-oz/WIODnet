test_that("obtain long network table", {

    ## network matrix
    w2002.IO <<- get(load("./wide_IO_w2002.rda"))
    
    mini.long <- getNetLong(w2002.IO)
    
    expect_equal(dim(mini.long), c(6888, 3))

})
