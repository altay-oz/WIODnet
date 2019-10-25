#' Calculate network scores for all industries
#'
#' @description Calculate network scores for all industries for each
#'     year from the long tables created with getWIOD function.
#'
#' @usage netWIOD(file.dir = "./wiod_long_data")
#'
#' @param file.dir The directory into which all long tables obtained from
#'     WIOD are written, Default: "./wiod_long_data"
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
netWIOD <- function(file.dir =  "./wiod_long_data") {
    ## creating the list of files
    wiod.long.file.name <- list.files(file.dir, pattern="^wiod_long", full.names = TRUE)

    if (length(wiod.long.file.name == 0)) {
        message("Downloading WIOD zip file and start network calculation.")
        getWIOD()
        message("Run panelWIOD() to obtain the csv panel data file.")
    }

    ## creating where all yearly network analysis are stored
    network.data.dir <- "./wiod_network_data"
    dir.create(network.data.dir, showWarnings = FALSE)
    
    ## call all functions above with this line, creating a final long file
    ## wiod_long_YEAR.csv to perform network analysis.
    lapply(wiod.long.file.name, netCalcWrite, network.data.dir = network.data.dir, ctry = 0)
}


#' Writing the calculated network file on disk.
#'
#' @description Calculate network scores for all industries for each
#'     year is written on disk with wiod_network_score_YEAR.rda
#'
#' @param wiod.long.file.name rda file containing the long network table
#'     to read.
#'
#' @param network.data.dir directory where all network calculation files
#'     are stored.
#'
#' @param ctry control variable if the analysis is country based, 0 or
#'     1.
#' 
netCalcWrite  <- function(wiod.long.file.name, network.data.dir, ctry) {

    if (ctry == 0) {
        year <- substr(wiod.long.file.name, 28, 31)
    } else if (ctry == 1) {
        year <- substr(wiod.long.file.name, 38, 41)
    }
    
    yearly.long.wiod <- get(load(wiod.long.file.name))
    
    nc <- netCalc(yearly.long.wiod, ctry)

    ## create file names
    if (ctry == 0) {
        file.name <- paste0(paste("wiod_network_scores", year, sep = "_"), ".rda")
    } else if (ctry == 1) {
        file.name <- paste0(paste("wiod_ctry_network_scores", year, sep = "_"), ".rda")
    }
        
    dir.file.name <- paste(network.data.dir, file.name, sep="/")
    ## write files with year
    save(nc, file = dir.file.name)

    ## just printing where we are.
    message(paste("The network file for the year", year, "is ready."))
    
}


#' Network calculation for each year
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
