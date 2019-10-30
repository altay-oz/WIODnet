#' Set directory names which store all files during data preparation
#'
#' @description Set the names of all directories which store long tables
#'     created during data preparation according to the ISIC code
#'     given. If ISIC is 0, then industries in WIOD are not aggregated,
#'     1; aggregation at ISIC 1 digit, 2; aggregation in the
#'     manufacturing industry with respect to the Eurostat technology
#'     intensity classification and leaving all other sectors as they
#'     are.
#' 
#' @param isic parameter to be given for the aggregation of the industries.
#' 
#' @return a list containing directories and final file name. 
#' 
setDir <- function(isic) {

    long.dir <- "./wiod_long_data"
    net.dir <- "./wiod_network_data"
    merge.dir <- "./yearly_merged_data"

    if (isic == 0) {
        ## as it is
        isic.long.dir  <- paste(long.dir, "isic_0", sep = "_")
        isic.net.dir <- paste(net.dir, "isic_0", sep = "_")
        isic.merge.dir <- paste(merge.dir, "isic_0", sep = "_")
        final.file.name <- "wiod_as_it_is_net_panel_2000_2014.csv"

    } else if (isic == 1) {
        ## isic single digit code
        isic.long.dir  <- paste(long.dir, "isic_1", sep = "_")
        isic.net.dir <- paste(net.dir, "isic_1", sep = "_")
        isic.merge.dir <- paste(merge.dir, "isic_1", sep = "_")
        final.file.name <- "wiod_isic_1_net_panel_2000_2014.csv"

    } else if (isic == 2) {
        ## manufacturing industry aggregated wrt the technology
        ## intensity categorization provided by Eurostat.
        isic.long.dir  <- paste(long.dir, "tech_int", sep = "_")
        isic.net.dir <- paste(net.dir, "tech_int", sep = "_")
        isic.merge.dir <- paste(merge.dir, "tech_int", sep = "_")
        final.file.name <- "wiod_manuf_net_panel_2000_2014.csv"

    }

    return(list(isic.long.dir, isic.net.dir, isic.merge.dir, final.file.name))
}
