#' Call all functions to download and create yearly long tables.
#'
#' @description Call various functions to download, unzip, divide and
#'     create yearly long tables from the original WIOD file. All long
#'     tables are also saved.
#'
#' @usage getWIOD(isic)
#'
#' @param isic parameter to be given for the aggregation of the industries.
#' 
#' @examples
#' # Downloading the zip file to the default directory (wiod_original_data)
#' \dontrun{getWIOD(2)}
#'
#' @import dplyr
#'
#' @export
#' 
getWIOD <- function(isic) {

    ## the directory where the original wiod zip data is downloaded.
    download.dir <- "./wiod_original_data"
    
    ## obtaning the name and creating the directory in which all long
    ## files are put
    dir.list <- setDir(isic)

    isic.long.dir <- dir.list[[1]]

    dir.create(isic.long.dir, showWarnings = FALSE)
    
    ## downloading the WIOD data. 
    downloadWIOD(download.dir)

    ## creating the list of files
    wiod.files <- list.files(download.dir, pattern="*.RData", full.names = TRUE)
    
    ## creating different long tables to be used in network analysis.
    lapply(wiod.files, getLongTables, isic.long.dir, isic)

}
