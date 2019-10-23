#' Create country based long tables and merge them to get a panel data set.
#'
#' @description Create country based long tables and merge them them to
#'     create a panel data in csv form based on downloaded and prapared
#'     WIOD data.
#' 
#' @usage getCountryWIOD(country.long.dir = "./wiod_long_data")
#'
#' @param country.long.dir directory where the long tables are recorded, Default: "./wiod_country_long_data"

#' @examples
#' # Merge all long tables using default values.
#' \dontrun{getCountryWIOD()}
#'
#' @export
#'
getCountryWIOD <- function(country.long.dir = "./wiod_country_long_data") {

    lapply(seq(2000, 2014), getCountryLong)

    wiod.ctry.long.file.name <- list.files(country.long.dir,
                                           pattern="^wiod_ctry_long", full.names = TRUE)

    ## creating where all yearly network analysis are stored
    network.ctry.data.dir <- "./wiod_ctry_network_data"
    dir.create(network.ctry.data.dir, showWarnings = FALSE)
    
    ## call all functions above with this line, creating a final long file
    ## wiod_long_YEAR.csv to perform network analysis.
    lapply(wiod.ctry.long.file.name, netCalcCtryWrite,
           network.ctry.data.dir = "./wiod_ctry_network_data")


    lapply(seq(2000, 2014), bindCtryFiles, long.dir = "./wiod_country_long_data",
                       net.dir = "./wiod_ctry_network_data",
                       merge.dir = "./yearly_ctry_merged_data")

    
    ## obtaining the last file to use in econometric study.
    ## rbinding all yearly network score VA value files
    final.ctry.wiod.df <- do.call(rbind,
                             lapply(list.files(path = merge.dir, full.names = TRUE),
                                    read.csv))

    write.csv(final.ctry.wiod.df, "wiod_ctry_net_panel_2000_2014.csv", row.names = FALSE)

    message("Country based panel data is created.")
}

# to use netCalcWrite once corrected
netCalcCtryWrite  <- function(wiod.long.file.name,
                              network.ctry.data.dir = "./wiod_ctry_network_data") {

    year <- substr(wiod.long.file.name, 41, 44)

    yearly.long.wiod <- get(load(wiod.long.file.name))

    nc <- netCtryCalc(yearly.long.wiod)

    ## create file names
    file.name <- paste0(paste("wiod_ctry_network_scores", year, sep = "_"), ".rda")

    dir.file.name <- paste(network.ctry.data.dir, file.name, sep="/")
    ## write files with year
    save(nc, file = dir.file.name)

    ## just printing where we are.
    message(paste("Country based network file for the year", year,
                  "is ready."))
    
}


netCtryCalc <- function(wiod.net.df) {
    
    ## removing 0.05 million $ connections.
    yearly.wiod <- wiod.net.df %>% filter(weight > 0.05)

    yearly.wiod <- ungroup(yearly.wiod)
    
    wiod.nodes.t <- yearly.wiod %>% select(target) %>% unique %>% rename(country = target)
    wiod.nodes.f <- yearly.wiod %>% select(source) %>% unique %>% rename(country=source)
    wiod.nodes <- rbind(wiod.nodes.t, wiod.nodes.f) %>% unique
    
    g <- graph.data.frame(yearly.wiod, directed = TRUE, vertices=wiod.nodes)

    ## node strength
    strength.all <- strength(g, vids = V(g), mode = c("all"), loops = TRUE)
    strength.all <- rownames_to_column(as.data.frame(strength.all), var = "country")
    strength.all[is.na(strength.all)] <- 0
    
    strength.out <- strength(g, vids = V(g), mode = c("out"), loops = TRUE)
    strength.out <- rownames_to_column(as.data.frame(strength.out), var = "country")
    strength.out[is.na(strength.out)] <- 0
    
    strength.in <- strength(g, vids = V(g), mode = c("in"), loops = TRUE)
    strength.in <- rownames_to_column(as.data.frame(strength.in), var = "country")
    strength.in[is.na(strength.in)] <- 0
    
    ## betweenness
    btw <- betweenness(g, v = V(g), directed = TRUE)
    btw <- rownames_to_column(as.data.frame(btw), var = "country")
    btw[is.na(btw)] <- 0
    
    ## page.rank
    page.rank <- page_rank(g, damping = 0.999)$vector
    page.rank <- rownames_to_column(as.data.frame(page.rank), var = "country")
    page.rank[is.na(page.rank)] <- 0
    net.scores <- Reduce(function(x, y) merge(x = x, y = y,
                                              by = "country", all.x = TRUE),
                         list(wiod.nodes, strength.all, strength.out,
                              strength.in, btw, page.rank))
    
    return(net.scores)
}


getCountryLong <- function(year, long.dir = "./wiod_long_data",
                        country.long.dir = "./wiod_country_long_data") {

    ## if the default wiod_long_data or the same long.dir directory is
    ## not present then download the original WIOD zip file and open it
    ## and create long tables at the designated long.dir.
    if(!file.exists(long.dir)) {
        getWIOD()
    }

    ## long.dir where all long data with country.ind is located
    ## country.long.dir where all long data only with country will be located
    dir.create(country.long.dir, showWarnings = FALSE)

    ## setting country.ind file names
    dom.int.trade.file <- paste0(long.dir, "/dom_int_trade_long_", year, ".rda")
    VA.file <- paste0(long.dir, "/VA_long_", year, ".rda")
    wiod.file  <- paste0(long.dir, "/wiod_long_", year, ".rda")       

    ## loading files
    dom.int <- get(load(dom.int.trade.file))
    VA <- get(load(VA.file))
    wiod <- get(load(wiod.file))

    ## extracting/creating and writing country base data
    dom.int.ctry <- dom.int %>% separate(country.ind, c("country", "ind"), sep = 3) %>% select(-ind)
    dom.int.ctry[is.na(dom.int.ctry)] <- 0
    dom.int.ctry %<>% group_by(country) %>% summarise_all(funs(sum))
    save(dom.int.ctry, file = paste0(country.long.dir, "/dom_int_ctry_long_", year, ".rda"))
    
    VA.ctry <- VA %>% separate(country.ind, c("country", "ind"), sep = 3) %>% select(-ind)
    VA.ctry[is.na(VA.ctry)] <- 0
    VA.ctry %<>% group_by(country) %>% summarise_all(funs(sum))
    save(VA.ctry, file = paste0(country.long.dir, "/VA_ctry_long_", year, ".rda"))

    wiod.ctry <- wiod %>% separate(source, c("source", "ind.source"), sep = 3) %>%
        select(-ind.source) %>% separate(target, c("target", "ind.target"), sep = 3) %>%
        select(-ind.target)
    wiod.ctry[is.na(wiod.ctry)] <- 0
    wiod.ctry %<>% group_by(source, target) %>% summarise_all(funs(sum))
    save(wiod.ctry, file = paste0(country.long.dir, "/wiod_ctry_long_", year, ".rda"))

    message(paste("Country based looooooong tables/files for the year", year,
                  "are ready."))
}


bindCtryFiles <- function(year,
                       long.dir = "./wiod_country_long_data",
                       net.dir = "./wiod_ctry_network_data",
                       merge.dir = "./yearly_ctry_merged_data") {
    ## bind the network scores, VA values and domestic and internation
    ## trade based on year and country.industry level.

    dir.create(merge.dir, showWarnings = FALSE)
    
    net.score.file <- paste0(net.dir, "/wiod_ctry_network_scores_", year, ".rda")
    dom.int.trade.file <- paste0(long.dir, "/dom_int_ctry_long_", year, ".rda")
    VA.file <- paste0(long.dir, "/VA_ctry_long_", year, ".rda")

    net.score.df <- get(load(net.score.file))
    dom.int.trade.df <- get(load(dom.int.trade.file))
    VA.df <- get(load(VA.file))
    
    ## from the longest one to the smallest df.  dom.int.trade.df
    ## comprises final consumption per country, (for example AUS.Z)
    yearly.net.score.dom.int.VA.df <- net.score.df %>% left_join(dom.int.trade.df) %>%
        left_join(VA.df)

    yearly.net.score.dom.int.VA.df$year <- year
    
    ##yearly.net.score.dom.int.VA.df  <- add_column(yearly.net.score.dom.int.VA.df,
      ##                                            year = year, .after = "country.ind")

    file.name <- paste0(merge.dir, "/net_score_dom_int_VA_", year, ".csv")

    write.csv(yearly.net.score.dom.int.VA.df, file.name, row.names = FALSE)

}
