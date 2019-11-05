yearly.mini.raw <- readRDS("./usa_fra_w2002.rds")

## French A01 VA, isic = 0, as it is.
raw.fra.a01.va <- yearly.mini.raw %>% filter(IndustryCode == "VA") %>%
    select("FRA1") %>% as.numeric

## French A agriculture, isic = 1
raw.fra.a.va <- yearly.mini.raw %>% filter(IndustryCode == "VA") %>%
    select(c("FRA1", "FRA2", "FRA3")) %>% rowSums %>% as.numeric

## French Low Tech VA, isic = 2 
raw.fra.lowtech.va <- yearly.mini.raw %>% filter(IndustryCode == "VA") %>%
    select(c("FRA5", "FRA6", "FRA7", "FRA8", "FRA9", "FRA22", "FRA23")) %>% rowSums %>% as.numeric

#############################################
## ISIC for manufacturing in 4 
isic <- 2

yearly.raw.ind.RNr.2 <- addTechIntensity(yearly.mini.raw, isic)
yearly.raw.2 <- yearly.raw.ind.RNr.2[[1]]
industry.RNr.2 <- yearly.raw.ind.RNr.2[[2]]

two.df.2 <- divideRawData(yearly.raw.2)

## other info grabbed from the raw data
w2002.comp.2 <- two.df.2[[2]]

## getting only the VA
va.df.2 <- getComplementary(w2002.comp.2, industry.RNr.2, "VA")

fra.a01.va.isic2 <- va.df.2 %>% filter(country.ind == "FRA.A01") %>%
    select(VA) %>% as.numeric

fra.lowtech.va.isic2 <- va.df.2 %>% filter(country.ind == "FRA.Low Tech") %>%
    select(VA) %>% as.numeric

#############################################
## ISIC single digit
isic <- 1

yearly.raw.ind.RNr.1 <- addTechIntensity(yearly.mini.raw, isic)
yearly.raw.1 <- yearly.raw.ind.RNr.1[[1]]
industry.RNr.1 <- yearly.raw.ind.RNr.1[[2]]

two.df.1 <- divideRawData(yearly.raw.1)

## other info grabbed from the raw data
w2002.comp.1 <- two.df.1[[2]]

## getting only the VA
va.df.1 <- getComplementary(w2002.comp.1, industry.RNr.1, "VA")

fra.a.va.isic1 <- va.df.1 %>% filter(country.ind == "FRA.A") %>% select(VA) %>% as.numeric

#############################################
## ISIC as it is in WIOD, no aggregation
isic <- 0

yearly.raw.ind.RNr.0 <- addTechIntensity(yearly.mini.raw, isic)
yearly.raw.0 <- yearly.raw.ind.RNr.0[[1]]
industry.RNr.0 <- yearly.raw.ind.RNr.0[[2]]

two.df.0 <- divideRawData(yearly.raw.0)

## other info grabbed from the raw data
w2002.comp.0 <- two.df.0[[2]]

## getting only the VA
va.df.0 <- getComplementary(w2002.comp.0, industry.RNr.0, "VA")

fra.a01.va.isic1 <- va.df.0 %>% filter(country.ind == "FRA.A01") %>% select(VA) %>% as.numeric

test_that("complementary matrix, testing VA", {
    ## dimension 
    expect_equal(dim(va.df.2), c(85, 2))
    ## check values
    expect_equivalent(raw.fra.a01.va , fra.a01.va.isic2)
    expect_equal(raw.fra.a.va, fra.a.va.isic1)
    expect_equal(raw.fra.lowtech.va, fra.lowtech.va.isic2)
    expect_equal(raw.fra.a01.va, fra.a01.va.isic1)
})



    
