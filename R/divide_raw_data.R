#' Divide the WIOD wide table into two separate df.
#'
#' @description The first one to be used in network calculations and the
#'     second one get the complementary information such as total
#'     intermediate consumption, taxes less subsidies on product,
#'     etc. on all economic sectors.
#' 
#' @param yearly.raw yearly raw data from the downloaded zip such as WIOT2011_October16_ROW.RData
#' 
#' @return a list conaining two data frames 
#'
#' @import dplyr
#'
divideRawData <- function(yearly.raw) {

    ## creating yearly complementary data to be used to get VA etc.
    yearly.complementary <- yearly.raw %>% filter(RNr > 64)
    ## obtaining
    ##   IndustryCode                                   IndustryDescription Country
    ## 1       II_fob                        Total intermediate consumption     TOT
    ## 2         TXSP                      taxes less subsidies on products     TOT
    ## 3      EXP_adj                       Cif/ fob adjustments on exports     TOT
    ## 4         PURR                  Direct purchases abroad by residents     TOT
    ## 5        PURNR Purchases on the domestic territory by non-residents      TOT
    ## 6           VA                           Value added at basic prices     TOT

    ## cleaning the data frame, removing unwanted columns in the raw file
    yearly.wide.IO <- yearly.raw %>% filter(RNr < 64)

    return(list(yearly.wide.IO, yearly.complementary))
}
