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
    ## creating where all yearly network analysis are stored
    network.data.dir <<- "./wiod_network_data"
    dir.create(network.data.dir, showWarnings = FALSE)
    
    ## call all functions above with this line, creating a final long file
    ## wiod_long_YEAR.csv to perform network analysis.
    lapply(wiod.long.file.name, netCalcWrite)
}


globalVariables(c("network.data.dir", "weight", "target", "V"))

#' Writing the calculated network file on disk.
#'
#' @description Calculate network scores for all industries for each
#'     year is written on disk with wiod_network_score_YEAR.rda
#'
#' @param wiod.long.file.name rda file containing the long network table
#'     to read.
#' 
netCalcWrite  <- function(wiod.long.file.name) {

    year <- substr(wiod.long.file.name, 28, 31)
    
    yearly.long.wiod <- get(load(wiod.long.file.name))
    
    nc <- netCalc(yearly.long.wiod)

    ## create file names
    file.name <- paste0(paste("wiod_network_scores", year, sep = "_"), ".rda")

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
netCalc <- function(wiod.net.df) {

    ## removing 0.05 million $ connections.
    yearly.wiod <- wiod.net.df %>% filter(weight > 0.05)

    ## ungroup
    yearly.wiod <- ungroup(yearly.wiod)
    
    wiod.nodes.t <- yearly.wiod %>% select(target) %>% unique %>% transmute(country.ind=target)
    wiod.nodes.f <- yearly.wiod %>% select(source) %>% unique %>% transmute(country.ind=source)
    wiod.nodes <- rbind(wiod.nodes.t, wiod.nodes.f) %>% unique

    g <- graph.data.frame(yearly.wiod, directed = TRUE, vertices=wiod.nodes)

    ## node strength
    strength.all <- strength(g, vids = V(g), mode = c("all"), loops = TRUE)
    strength.all <- rownames_to_column(as.data.frame(strength.all), var = "country.ind")
    strength.all[is.na(strength.all)] <- 0
    
    strength.out <- strength(g, vids = V(g), mode = c("out"), loops = TRUE)
    strength.out <- rownames_to_column(as.data.frame(strength.out), var = "country.ind")
    strength.out[is.na(strength.out)] <- 0
    
    strength.in <- strength(g, vids = V(g), mode = c("in"), loops = TRUE)
    strength.in <- rownames_to_column(as.data.frame(strength.in), var = "country.ind")
    strength.in[is.na(strength.in)] <- 0
    
    ## betweenness
    btw <- betweenness(g, v = V(g), directed = TRUE)
    btw <- rownames_to_column(as.data.frame(btw), var = "country.ind")
    btw[is.na(btw)] <- 0
    
    ## page.rank
    page.rank <- page_rank(g, damping = 0.999)$vector
    page.rank <- rownames_to_column(as.data.frame(page.rank), var = "country.ind")
    page.rank[is.na(page.rank)] <- 0
    
    ## eigen centrality
    ## https://lists.nongnu.org/archive/html/igraph-help/2015-11/msg00020.html
    eigen.cent <- eigen_centrality(g, directed = TRUE)$vector
    eigen.cent <- rownames_to_column(as.data.frame(eigen.cent), var = "country.ind")
    eigen.cent[is.na(eigen.cent)] <- 0
    
    net.scores <- Reduce(function(x, y) merge(x = x, y = y, by = "country.ind",
                                              all.x = TRUE),
                         list(wiod.nodes, strength.all,
                              strength.out, strength.in, btw,
                              page.rank, eigen.cent))
    
    return(net.scores)
}
