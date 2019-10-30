context("calculation of domestic and international trade")

## long network table
w2002.net.long <- readRDS("./net_long_w2002.rds")

dom.int.fra.usa <- domIntTrade(w2002.net.long)

z.out <- dom.int.fra.usa %>% select(country.ind, dom.out.weight, int.out.weight,
                                    dom.Z.weight, int.Z.weight) %>%
    filter(country.ind %in% c("USA.Z", "FRA.Z"))

z.out[is.na(z.out)]  <- 0

test_that("dimensions and NAs", {
    expect_equal(dim(dom.int.fra.usa), c(84, 7))
    expect_equal(z.out[1, 2], 0)
    expect_equal(z.out[2, 4], 0)
})
