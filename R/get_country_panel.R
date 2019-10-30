#' Create country based long tables and merge them to get a panel data set.
#'
#' @description Create country based long tables and merge them them to
#'     create a panel data in csv form based on downloaded and prapared
#'     WIOD data. If there is no WIOD data downloaded, this function
#'     will donwload it, unzip it and prepare long tables.
#' 
#' @usage getCountryWIOD()
#'
#' @examples
#' # Create country based panel data.
#' \dontrun{getCountryWIOD()}
#'
#' @export
#'
getCountryWIOD <- function() {

    ## obtaining list of directories
    dir.isic.0 <- setDir(0)
    dir.isic.1 <- setDir(1)
    dir.isic.2 <- setDir(2)

    long.dirs <- c(dir.isic.0[[1]], dir.isic.1[[1]], dir.isic.2[[1]])

    ## check if any getWIOD(isic) function is allready run.
    for (dir in long.dirs) {
        if(file.exists(dir)) {
            long.dir <- dir
            break
        }
    }

    ## if the default wiod_long_data or the same long.dir directory is
    ## not present then download the original WIOD zip file and open it
    ## and create long tables at the designated long.dir.
    if(!exists("long.dir")) {
        message("There is no wiod long data directory.")
        message("Starting to download the original zip file.")
        ## creating isic 1 digit tables, it is faster. 
        getWIOD(1)
        long.dir <- dir.isic.1[[1]]
    }

    ctry.long.dir <- "./wiod_country_long_data"
    ctry.merge.dir <- "./wiod_country_merge"
    
    dir.create(ctry.long.dir, showWarnings = FALSE)
    dir.create(ctry.merge.dir, showWarnings = FALSE)
    
    lapply(seq(2000, 2014), getCtryLong, long.dir, ctry.long.dir)

    wiod.ctry.long.file.name <- list.files(ctry.long.dir,
                                           pattern="^wiod_ctry_long", full.names = TRUE)

    ## creating where all yearly network analysis are stored
    network.ctry.data.dir <- "./wiod_country_network_data"
    dir.create(network.ctry.data.dir, showWarnings = FALSE)
    
    ## creating yearly network calculation files where all nodes are countries.
    lapply(wiod.ctry.long.file.name, netCalcWrite,
           net.data.dir = network.ctry.data.dir, ctry = 1)

    ## binding all files yearly.
    lapply(seq(2000, 2014), bindFiles,
           long.dir = ctry.long.dir,
           net.dir = network.ctry.data.dir,
           merge.dir = ctry.merge.dir,
           ctry = 1)
    
    ## obtaining the last file to use in econometric study.
    ## rbinding all yearly network score VA values etc.
    final.ctry.wiod.df <- do.call(rbind,
                             lapply(list.files(path = ctry.merge.dir, full.names = TRUE),
                                    readRDS))

    write.csv(final.ctry.wiod.df, "wiod_ctry_net_panel_2000_2014.csv", row.names = FALSE)
    message("Country based panel data wiod_ctry_net_panel_2000_2014.csv is ready")
}


getCtryLong <- function(year, long.dir, country.long.dir) {

    ## long.dir where all long data with country.ind is located
    ## country.long.dir where all long data only with country will be located
    dir.create(country.long.dir, showWarnings = FALSE)

    ## setting country.ind file names
    dom.int.trade.file <- paste0(long.dir, "/dom_int_trade_long_", year, ".rds")
    VA.file <- paste0(long.dir, "/VA_long_", year, ".rds")
    wiod.file  <- paste0(long.dir, "/wiod_long_", year, ".rds")       

    ## reading files
    dom.int <- readRDS(dom.int.trade.file)
    VA <- readRDS(VA.file)
    wiod <- readRDS(wiod.file)

    ## extracting/creating and writing country base data
    dom.int.ctry <- dom.int %>% separate(country.ind, c("country", "ind"), sep = 3) %>% select(-ind)
    dom.int.ctry[is.na(dom.int.ctry)] <- 0
    dom.int.ctry %<>% group_by(country) %>% summarise_all(funs(sum))
    saveRDS(dom.int.ctry, file = paste0(country.long.dir, "/dom_int_ctry_long_", year, ".rds"))
    
    VA.ctry <- VA %>% separate(country.ind, c("country", "ind"), sep = 3) %>% select(-ind)
    VA.ctry[is.na(VA.ctry)] <- 0
    VA.ctry %<>% group_by(country) %>% summarise_all(funs(sum))
    saveRDS(VA.ctry, file = paste0(country.long.dir, "/VA_ctry_long_", year, ".rds"))

    wiod.ctry <- wiod %>% separate(source, c("source", "ind.source"), sep = 3) %>%
        select(-ind.source) %>% separate(target, c("target", "ind.target"), sep = 3) %>%
        select(-ind.target)
    wiod.ctry[is.na(wiod.ctry)] <- 0
    wiod.ctry %<>% group_by(source, target) %>% summarise_all(funs(sum))
    saveRDS(wiod.ctry, file = paste0(country.long.dir, "/wiod_ctry_long_", year, ".rds"))

    message(paste("Country based looooooong tables/files for the year", year,
                  "are ready."))
}
