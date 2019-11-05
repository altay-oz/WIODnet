#' Calculate network scores for all industries
#'
#' @description Calculate network scores for all industries for each
#'     year from the long tables created with getWIOD function.
#'
#' @usage netWIOD(isic)
#'
#' @param isic parameter to be given for the aggregation of the industries.
#' 
#' @importFrom igraph graph.data.frame
#'
#' @importFrom igraph strength
#'
#' @importFrom igraph betweenness
#'
#' @importFrom igraph page_rank
#'
#' @importFrom igraph eigen_centrality
#'
#' @importFrom igraph V
#'
#' @importFrom tibble rownames_to_column
#' 
#' @export
#' 
netWIOD <- function(isic) {

    dir.list <- setDir(isic)

    isic.long.dir <- dir.list[[1]]
    isic.net.dir <- dir.list[[2]]
    isic.merge.dir <- dir.list[[3]]
    final.file.name <- dir.list[[4]]

    ## creating the list of files
    wiod.long.file.name <- list.files(isic.long.dir, pattern="^wiod_long",
                                      full.names = TRUE)
    
    if (length(wiod.long.file.name) == 0) {
        message("Long files are not found.")
        message("Once long files are made network calculations will start.")
        getWIOD(isic)
        ## obtaining the new and non-empty list of files to be used in
        ## network calculations
        wiod.long.file.name <- list.files(isic.long.dir,
                                          pattern="^wiod_long",
                                          full.names = TRUE)
    }

    ## creating where all yearly network analysis are stored
    dir.create(isic.net.dir, showWarnings = FALSE)
    
    ## call the functions which calculate and write long files into the
    ## net.dir.
    lapply(wiod.long.file.name, netCalcWrite, net.data.dir = isic.net.dir, ctry = 0)

    message("Run panelWIOD(isic) to obtain the csv panel data file.")
}

#' Writing the calculated network file on disk.
#'
#' @description Calculate network scores for all industries for each
#'     year is written on disk with wiod_network_score_YEAR.rda
#'
#' @param wiod.long.file.name rda file containing the long network table
#'     to read.
#'
#' @param net.data.dir directory where all network calculation files
#'     are stored.
#'
#' @param ctry control variable if the analysis is country based, 0 or
#'     1.
#' 
netCalcWrite  <- function(wiod.long.file.name, net.data.dir, ctry) {

    ## measuring the length of the file name and obtaining where year
    ## info is on the file name.
    file.name.length <- str_length(wiod.long.file.name)
    year.start <- file.name.length - 7
    year.end <- file.name.length - 4

    yearly.long.wiod <- readRDS(wiod.long.file.name)
    
    if (ctry == 0) {
        year <- substr(wiod.long.file.name, year.start, year.end)
        
        ## remove the final market Z from the network calculation
        yearly.long.net.wiod <- yearly.long.wiod %>% separate(target, c("target.country",
                                                                        "target.industry"),
                                                              sep = "\\.") %>% 
            filter(target.industry != "Z") %>% unite("target",
                                                     "target.country",
                                                     "target.industry", sep = ".")
    } else if (ctry == 1) {
        year <- substr(wiod.long.file.name, year.start, year.end)

        yearly.long.net.wiod <- yearly.long.wiod

    }
    
    nc <- netCalc(yearly.long.net.wiod, ctry)

    ## create file names
    if (ctry == 0) {
        file.name <- paste0(paste("wiod_network_scores", year, sep = "_"), ".rds")
    } else if (ctry == 1) {
        file.name <- paste0(paste("wiod_ctry_network_scores", year, sep = "_"), ".rds")
    }
        
    dir.file.name <- paste(net.data.dir, file.name, sep="/")
    ## write files with year
    saveRDS(nc, file = dir.file.name)

    ## just printing where we are.
    message(paste("The network file for the year", year, "is ready."))
    
}


#' Network calculation for a data frame with source - target - weight.
#'
#' @description Calculate network scores of long files obtained from
#'     WIOD. The weight is filtered with 0.05 (for now).
#'
#' @param wiod.net.df the long table data frame on which all network
#'     calculations are done.
#'
#' @param ctry control variable if the analysis is country based, 0 or
#'     1.
#' 
netCalc <- function(wiod.net.df, ctry) {

    ## removing 0.05 million $ connections.
    yearly.wiod <- wiod.net.df %>% filter(weight > 0.05)

    ## ungroup
    yearly.wiod <- ungroup(yearly.wiod)

    wiod.nodes.t <- yearly.wiod %>% select(target) %>% unique %>% rename(node.name = target)
    wiod.nodes.f <- yearly.wiod %>% select(source) %>% unique %>% rename(node.name = source)
    wiod.nodes <- rbind(wiod.nodes.t, wiod.nodes.f) %>% unique


    if (ctry == 1) {
        node.type <- "country"
        names(wiod.nodes) <- "country"
    } else if (ctry == 0) {
        node.type <- "country.ind"
        names(wiod.nodes) <- "country.ind"
    }
    
    g <- graph.data.frame(yearly.wiod, directed = TRUE, vertices=wiod.nodes)

    ## node strength
    strength.all <- strength(g, vids = V(g), mode = c("all"), loops = TRUE)
    strength.all <- rownames_to_column(as.data.frame(strength.all), var = node.type)
    strength.all[is.na(strength.all)] <- 0
    
    strength.out <- strength(g, vids = V(g), mode = c("out"), loops = TRUE)
    strength.out <- rownames_to_column(as.data.frame(strength.out), var = node.type)
    strength.out[is.na(strength.out)] <- 0
    
    strength.in <- strength(g, vids = V(g), mode = c("in"), loops = TRUE)
    strength.in <- rownames_to_column(as.data.frame(strength.in), var = node.type)
    strength.in[is.na(strength.in)] <- 0
    
    ## betweenness
    btw <- betweenness(g, v = V(g), directed = TRUE)
    btw <- rownames_to_column(as.data.frame(btw), var = node.type)
    btw[is.na(btw)] <- 0
    
    ## page.rank
    page.rank <- page_rank(g, damping = 0.999)$vector
    page.rank <- rownames_to_column(as.data.frame(page.rank), var = node.type)
    page.rank[is.na(page.rank)] <- 0
    
    ## eigen centrality
    ## https://lists.nongnu.org/archive/html/igraph-help/2015-11/msg00020.html
    eigen.cent <- eigen_centrality(g, directed = TRUE)$vector
    eigen.cent <- rownames_to_column(as.data.frame(eigen.cent), var = node.type)
    eigen.cent[is.na(eigen.cent)] <- 0

    net.scores <- Reduce(function(x, y) merge(x = x, y = y, by = node.type,
                                              all.x = TRUE),
                         list(wiod.nodes, strength.all,
                              strength.out, strength.in, btw,
                              page.rank, eigen.cent))
    
    return(net.scores)
}

