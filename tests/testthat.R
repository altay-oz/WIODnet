library(testthat)
library(WIODnet)

## loading the test data of the year 2002 WIOD for exchanges
## between FRA and USA
yearly.mini.raw <<- get(load("./w2002.rda"))
## list to be divided
w2002.divided <<- get(load("./divided_w2002.rda"))
## complementary data
w2002.comp <<- get(load("./wide_comp_w2002.rda"))
## network matrix
w2002.IO <<- get(load("./wide_IO_w2002.rda"))

## rnr_industry.rda

## long network table
w2002.net.long <<- get(load("./net_long_w2002.rda"))



test_check("WIODnet")
