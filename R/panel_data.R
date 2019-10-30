#' Merge all to create a panel data for analysis.
#'
#' @description Merge all long tables to create a panel data in csv form
#'     based on WIOD downloaded data.
#' 
#' @usage panelWIOD(isic)
#'
#' @param isic parameter to be given for the aggregation of the industries.
#' 
#' @examples
#' # Merge all long tables using default values.
#' \dontrun{panelWIOD(2)}
#'
#' @export
#' 
panelWIOD <- function(isic) {

    ## Creating directories wrt isic code given and check if they are
    ## created if not it starts to call all necessary functions to get
    ## the final panel data.
    dir.list <- setDir(isic)

    isic.long.dir <- dir.list[[1]]
    isic.net.dir <- dir.list[[2]]
    isic.merge.dir <- dir.list[[3]]
    final.file.name <- dir.list[[4]]

    dir.create(isic.merge.dir, showWarnings = FALSE)

    ## creating the list of files after network analysis
    wiod.net.file.name <- list.files(isic.net.dir, pattern="^wiod_network_scores",
                                      full.names = TRUE)

    ## if there are no net files then netWIOD is run
    if (length(wiod.net.file.name) == 0) {
        message("Network score files are not found.")
        message("Once network score files are created merging will start.")
        netWIOD(isic)
    }

    ## creating all yearly binded network score and VA value files and
    ## stored at yearly.net.VA.dir
    lapply(seq(2000,2014), bindFiles,
           long.dir = isic.long.dir, 
           net.dir = isic.net.dir,
           merge.dir = isic.merge.dir,
           ctry = 0)
        
    ## obtaining the last file to use in econometric study.
    ## rbinding all yearly network score VA value files
    final.wiod.df <- do.call(rbind,
                             lapply(list.files(path = isic.merge.dir, full.names = TRUE),
                                    readRDS))

    write.csv(final.wiod.df, final.file.name, row.names = FALSE)
    message(paste0("The file ", final.file.name, " is ready"))
}

#' Bind all files to create a yearly panel data.
#'
#' @description Merge all long tables to create a panel data in csv form
#'     for each year.
#'
#' @param year between 2000 and 2014, WIOD years.
#' 
#' @param long.dir directory where the long tables are recorded, Default: "./wiod_long_data"
#'
#' @param net.dir directory where the long tables on network
#'     calculations are recorded, Default: "./wiod_network_data"
#'
#' @param merge.dir directory where the long tables are all merged by
#'     year basis, Default: "./yearly_merged_data"
#'
#' @param ctry control variable if the process is based on countries
#' 
bindFiles <- function(year, long.dir, net.dir, merge.dir, ctry) {
    ## bind the network scores, VA values and domestic and internation
    ## trade based on year and country.industry level.

    if (ctry == 0) {
        net.score.file <- paste0(net.dir, "/wiod_network_scores_", year, ".rds")
        dom.int.trade.file <- paste0(long.dir, "/dom_int_trade_long_", year, ".rds")
        VA.file <- paste0(long.dir, "/VA_long_", year, ".rds")

        file.name <- paste0(merge.dir, "/net_score_dom_int_VA_", year, ".rds")
    } else if (ctry == 1) {
        net.score.file <- paste0(net.dir, "/wiod_ctry_network_scores_", year, ".rds")
        dom.int.trade.file <- paste0(long.dir, "/dom_int_ctry_long_", year, ".rds")
        VA.file <- paste0(long.dir, "/VA_ctry_long_", year, ".rds")

        file.name <- paste0(merge.dir, "/net_score_dom_int_VA_", year, ".rds")
    }
    
    net.score.df <- readRDS(net.score.file)
    dom.int.trade.df <- readRDS(dom.int.trade.file)
    VA.df <- readRDS(VA.file)
    
    ## from the longest one to the smallest df.  dom.int.trade.df
    ## comprises final consumption per country, (for example AUS.Z)

    if (ctry == 0) {
        yearly.net.score.dom.int.VA.df <- net.score.df %>%
            left_join(dom.int.trade.df, by = "country.ind") %>%
            left_join(VA.df, by = "country.ind")
    } else if (ctry == 1) {
        yearly.net.score.dom.int.VA.df <- net.score.df %>%
            left_join(dom.int.trade.df, by = "country") %>%
            left_join(VA.df, by = "country")
    }        
    
    yearly.net.score.dom.int.VA.df$year <- year
    
    ##yearly.net.score.dom.int.VA.df  <- add_column(yearly.net.score.dom.int.VA.df,
      ##                                            year = year, .after = "country.ind")

    saveRDS(yearly.net.score.dom.int.VA.df, file.name)
}
