#' Merge all to create a panel data for analysis.
#'
#' @description Merge all long tables to create a panel data in csv form
#'     based on WIOD downloaded data.
#' 
#' @usage panelWIOD(long.dir = "./wiod_long_data", net.dir =
#'     "./wiod_network_data", merge.dir = "./yearly_merged_data")
#'
#' @param long.dir directory where the long tables are recorded,
#'     Default: "./wiod_long_data"
#'
#' @param net.dir directory where the long tables on network
#'     calculations are recorded, Default: "./wiod_network_data"
#'
#' @param merge.dir directory where the long tables are all merged by
#'     year basis, Default: "./yearly_merged_data"
#' 
#' @examples
#' # Merge all long tables using default values.
#' \dontrun{panelWIOD()}
#'
#' @export
panelWIOD <- function(long.dir = "./wiod_long_data",
                      net.dir = "./wiod_network_data",
                      merge.dir = "./yearly_merged_data",
                      isic) {

    user.long.dir  <- long.dir
    user.net.dir <- net.dir
    user.merge.dir <- merge.dir

    dir.create(user.merge.dir, showWarnings = FALSE)

    ## if there is no network directory then netWIOD did not run yet.
    if (!file.exists(net.dir)) {
        message("There is no network directory, running network calculations.")
        netWIOD(long.dir, isic)
    }
    
    ## creating all yearly binded network score and VA value files and
    ## stored at yearly.net.VA.dir
    lapply(seq(2000,2014), bindFiles,
           long.dir = user.long.dir, 
           net.dir = user.net.dir,
           merge.dir = user.merge.dir,
           ctry = 0)
        
    ## obtaining the last file to use in econometric study.
    ## rbinding all yearly network score VA value files
    final.wiod.df <- do.call(rbind,
                             lapply(list.files(path = user.merge.dir, full.names = TRUE),
                                    read.csv))

    write.csv(final.wiod.df, "wiod_manuf_net_panel_2000_2014.csv", row.names = FALSE)
    message("The file wiod_manuf_net_panel_2000_2014.csv is ready")
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
bindFiles <- function(year,
                      long.dir = "./wiod_long_data",
                      net.dir = "./wiod_network_data",
                      merge.dir = "./yearly_merged_data",
                      ctry = 0) {
    ## bind the network scores, VA values and domestic and internation
    ## trade based on year and country.industry level.

    if (ctry == 0) {
        net.score.file <- paste0(net.dir, "/wiod_network_scores_", year, ".rda")
        dom.int.trade.file <- paste0(long.dir, "/dom_int_trade_long_", year, ".rda")
        VA.file <- paste0(long.dir, "/VA_long_", year, ".rda")

        file.name <- paste0(merge.dir, "/net_score_dom_int_VA_", year, ".csv")
    } else if (ctry == 1) {
        net.score.file <- paste0(net.dir, "/wiod_ctry_network_scores_", year, ".rda")
        dom.int.trade.file <- paste0(long.dir, "/dom_int_ctry_long_", year, ".rda")
        VA.file <- paste0(long.dir, "/VA_ctry_long_", year, ".rda")

        file.name <- paste0(merge.dir, "/net_score_dom_int_VA_", year, ".csv")
    }
    
    net.score.df <- get(load(net.score.file))
    dom.int.trade.df <- get(load(dom.int.trade.file))
    VA.df <- get(load(VA.file))
    
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

    write.csv(yearly.net.score.dom.int.VA.df, file.name, row.names = FALSE)
}
