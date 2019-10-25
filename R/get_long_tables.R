#' Generate long tables from the original WIOD zip file.
#'
#' @description Call various functions to prepare long tables which are
#'     written on to the disk.
#'
#' @param file.name yearly file name obtained from the download directory
#' 
#' @import dplyr
#'
getLongTables <- function(file.name, dir.to.write) {

    yearly.raw <- get(load(file.name))
    
    year <- unique(yearly.raw$Year)

    ## using the raw file to change it according to industry.RNr and
    ## obtaining industry.RNr
    yearly.raw.ind.RNr <- addTechIntensity(yearly.raw)
    yearly.raw <- yearly.raw.ind.RNr[[1]]
    industry.RNr <- yearly.raw.ind.RNr[[2]]
    
    ## obtaining the wide df for network transformation and other data
    two.df <- divideRawData(yearly.raw)

    ## the wide df to be used to obtain network long df
    yearly.IO <- two.df[[1]]
    ## getting the long df
    net.long <- getNetLong(yearly.IO, industry.RNr)

    ## other info grabbed from the raw data
    yearly.complementary <- two.df[[2]]
    
    ## IndustryCode                                   IndustryDescription Country
    ##     II_fob                        Total intermediate consumption     TOT
    ##       TXSP                      taxes less subsidies on products     TOT
    ##    EXP_adj                       Cif/ fob adjustments on exports     TOT
    ##       PURR                  Direct purchases abroad by residents     TOT
    ##      PURNR Purchases on the domestic territory by non-residents      TOT
    ##         VA                           Value added at basic prices     TOT

    ## getting only the VA
    VA.df <- getComplementary(yearly.complementary, industry.RNr, "VA")

    ## and the other info | uncomment them if needed and don't forget to
    ## bind them
    ## II_fob.df <- get.complementary(yearly.complementary, "II_fob")
    ## TXSP.df <- get.complementary(yearly.complementary, "TXSP")
    ## EXP_adj.df <- get.complementary(yearly.complementary, "EXP_adj")
    ## PURR.df <- get.complementary(yearly.complementary, "PURR")
    ## PURNR.df <- get.complementary(yearly.complementary, "PURNR")

    ## obtaining domestic and international trade in and out weight
    ## values for each country.industry
    dom.int.weights <- domIntTrade(net.long)
    
    ## create file names
    file.name.net <- paste0(paste("wiod_long", year, sep = "_"), ".rda")
    file.name.VA <- paste0(paste("VA_long", year, sep = "_"), ".rda")
    file.name.dom.int <- paste0(paste("dom_int_trade_long", year, sep = "_"), ".rda")
    
    ## writing all dataframes
    writeFile(net.long, file.name.net, dir.to.write)
    writeFile(VA.df, file.name.VA, dir.to.write)
    writeFile(dom.int.weights, file.name.dom.int, dir.to.write)
    
    ## just printing where we are.
    message(paste("Year finished:", year))

}

writeFile <- function(df, file.name, dir.to.write) {
    dir.file <- paste(dir.to.write, file.name, sep="/")
    save(df, file = dir.file)
}


skip_if_no_file <- function() {
    if(TRUE) {
        skip("Calling all tested functions to write them as rda files for each year.")
    }
}

