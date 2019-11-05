## reading the toy data set
## international here means only to FRANCE or USA for each other.
yearly.mini.raw <- readRDS("./usa_fra_w2002.rds")

raw.w2002.usa.a01 <- yearly.mini.raw %>% filter(Country == "USA", IndustryCode == "A01")

raw.w2002.usa.a01.dom.Z  <- raw.w2002.usa.a01 %>%
    select(USA57, USA58, USA59, USA60, USA61) %>% rowSums %>% as.numeric

raw.w2002.usa.a01.int.Z <- raw.w2002.usa.a01 %>%
    select(FRA57, FRA58, FRA59, FRA60, FRA61) %>% rowSums %>% as.numeric

raw.w2002.usa.a01.dom.prod  <- raw.w2002.usa.a01 %>%
    select(starts_with("USA") ) %>%
    rowSums %>% as.numeric

raw.w2002.usa.a01.int.prod  <- raw.w2002.usa.a01 %>%
    select(starts_with("FRA") ) %>%
    rowSums %>% as.numeric

#############################################
## ISIC for manufacturing in 4 
isic <- 2

yearly.raw.ind.RNr.2 <- addTechIntensity(yearly.mini.raw, isic)
yearly.raw.2 <- yearly.raw.ind.RNr.2[[1]]
industry.RNr.2 <- yearly.raw.ind.RNr.2[[2]]

two.df.2 <- divideRawData(yearly.raw.2)

## other info grabbed from the raw data
w2002.IO.2 <- two.df.2[[1]]

## creating long network table (from-to-weight)
w2002.net.long <- getNetLong(w2002.IO.2, industry.RNr.2)

dom.int.fra.usa <- domIntTrade(w2002.net.long)

## checking the if Z (final consumption market) production is NA, it should be.
z.out <- dom.int.fra.usa %>% select(country.ind, dom.out.weight, int.out.weight,
                                    dom.Z.weight, int.Z.weight) %>%
    filter(country.ind %in% c("USA.Z", "FRA.Z"))

## USA A01 production domestic, international for intermediate and final market 
usa.a01 <- dom.int.fra.usa %>% select(country.ind, dom.out.weight, int.out.weight,
                                    dom.Z.weight, int.Z.weight) %>%
    filter(country.ind %in% c("USA.A01"))
## final market production
usa.a01.Z.int <- usa.a01 %>% select(int.Z.weight) %>% as.numeric
usa.a01.Z.dom <- usa.a01 %>% select(dom.Z.weight) %>% as.numeric
## domestic and international intermediate market production
usa.a01.dom.out <- usa.a01 %>% select(dom.out.weight) %>% as.numeric
usa.a01.int.out <- usa.a01 %>% select(int.out.weight) %>% as.numeric

test_that("dimensions and NAs", {
    expect_equal(dim(dom.int.fra.usa), c(84, 7))
    expect_true(is.na(z.out[1, 2]))
    expect_true(is.na(z.out[2, 4]))
})


test_that("values are OK", {
    expect_equal(raw.w2002.usa.a01.dom.Z, usa.a01.Z.dom)
    expect_equal(raw.w2002.usa.a01.int.Z, usa.a01.Z.int)
    expect_equal(raw.w2002.usa.a01.dom.prod, usa.a01.dom.out)
    expect_equal(raw.w2002.usa.a01.int.prod, usa.a01.int.out)
})

