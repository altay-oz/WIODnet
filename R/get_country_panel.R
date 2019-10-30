#' Create country based long tables and merge them to get a panel data set.
#'
#' @description Create country based long tables and merge them them to
#'     create a panel data in csv form based on downloaded and prapared
#'     WIOD data. If there is no WIOD data downloaded, this function
#'     will donwload it, unzip it and prepare long tables.
#' 
#' @usage getCountryWIOD(long.dir = "./wiod_long_data",
#'     country.long.dir = "./wiod_ctry_long_data", ctry.merge.dir =
#'     "./yearly_ctry_merged_data")
#'
#' @param long.dir directory where the long tables are already obtained,
#'     Default: "./wiod_long_data"
#' 
#' @param country.long.dir directory where the long tables are recorded,
#'     Default: "./wiod_country_long_data"
#'
#' @param ctry.merge.dir directory where all tables are merged for each
#'     year, Default: "./wiod_country_long_data"
#' 
#' @examples
#' # Create country based panel data.
#' \dontrun{getCountryWIOD()}
#'
#' @export
#'
getCountryWIOD <- function(long.dir =  "./wiod_long_data",
                           country.long.dir = "./wiod_ctry_long_data",
                           ctry.merge.dir = "./yearly_ctry_merged_data") {

    ## if the default wiod_long_data or the same long.dir directory is
    ## not present then download the original WIOD zip file and open it
    ## and create long tables at the designated long.dir.
    if(!file.exists(long.dir)) {
        message("There is no wiod long data directory.")
        message("Starting to download the original zip file and creating long tables.")
        getWIOD()
    }

    dir.create(country.long.dir, showWarnings = FALSE)
    dir.create(ctry.merge.dir, showWarnings = FALSE)
    
    lapply(seq(2000, 2014), getCtryLong, long.dir = "./wiod_long_data",
                        country.long.dir = "./wiod_ctry_long_data")

    wiod.ctry.long.file.name <- list.files(country.long.dir,
                                           pattern="^wiod_ctry_long", full.names = TRUE)

    ## creating where all yearly network analysis are stored
    network.ctry.data.dir <- "./wiod_ctry_network_data"
    dir.create(network.ctry.data.dir, showWarnings = FALSE)
    
    ## creating yearly network calculation files where all nodes are countries.
    lapply(wiod.ctry.long.file.name, netCalcWrite,
           network.data.dir = "./wiod_ctry_network_data", ctry = 1)

    ## binding all files yearly.
    lapply(seq(2000, 2014), bindFiles,
           long.dir = "./wiod_ctry_long_data",
           net.dir = "./wiod_ctry_network_data",
           merge.dir = ctry.merge.dir,
           ctry = 1)

    ## obtaining the last file to use in econometric study.
    ## rbinding all yearly network score VA values etc.
    final.ctry.wiod.df <- do.call(rbind,
                             lapply(list.files(path = ctry.merge.dir, full.names = TRUE),
                                    read.csv))

    write.csv(final.ctry.wiod.df, "wiod_ctry_net_panel_2000_2014.csv", row.names = FALSE)
    message("Country based panel data wiod_ctry_net_panel_2000_2014.csv is ready")
}


getCtryLong <- function(year,
                        long.dir = "./wiod_long_data",
                        country.long.dir = "./wiod_ctry_long_data") {

    ## long.dir where all long data with country.ind is located
    ## country.long.dir where all long data only with country will be located
    dir.create(country.long.dir, showWarnings = FALSE)

    ## setting country.ind file names
    dom.int.trade.file <- paste0(long.dir, "/dom_int_trade_long_", year, ".rda")
    VA.file <- paste0(long.dir, "/VA_long_", year, ".rda")
    wiod.file  <- paste0(long.dir, "/wiod_long_", year, ".rda")       

    ## reading files
    dom.int <- readRDS(dom.int.trade.file)
    VA <- readRDS(VA.file)
    wiod <- readRDS(wiod.file)

    ## extracting/creating and writing country base data
    dom.int.ctry <- dom.int %>% separate(country.ind, c("country", "ind"), sep = 3) %>% select(-ind)
    dom.int.ctry[is.na(dom.int.ctry)] <- 0
    dom.int.ctry %<>% group_by(country) %>% summarise_all(funs(sum))
    saveRDS(dom.int.ctry, file = paste0(country.long.dir, "/dom_int_ctry_long_", year, ".rda"))
    
    VA.ctry <- VA %>% separate(country.ind, c("country", "ind"), sep = 3) %>% select(-ind)
    VA.ctry[is.na(VA.ctry)] <- 0
    VA.ctry %<>% group_by(country) %>% summarise_all(funs(sum))
    saveRDS(VA.ctry, file = paste0(country.long.dir, "/VA_ctry_long_", year, ".rda"))

    wiod.ctry <- wiod %>% separate(source, c("source", "ind.source"), sep = 3) %>%
        select(-ind.source) %>% separate(target, c("target", "ind.target"), sep = 3) %>%
        select(-ind.target)
    wiod.ctry[is.na(wiod.ctry)] <- 0
    wiod.ctry %<>% group_by(source, target) %>% summarise_all(funs(sum))
    saveRDS(wiod.ctry, file = paste0(country.long.dir, "/wiod_ctry_long_", year, ".rda"))

    message(paste("Country based looooooong tables/files for the year", year,
                  "are ready."))
}
