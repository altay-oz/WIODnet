#' Call all functions to download and create yearly long tables.
#'
#' @description Call various functions to download, unzip, divide and
#'     create yearly long tables from the original WIOD file. All long
#'     tables are also saved.
#'
#' @usage getWIOD(dir = "./wiod_original_data")
#'
#' @param dir The directory into which the original WIOD zip file is
#'     downloaded and unziped, Default: "./wiod_original_data"
#'
#' @examples
#' # Downloading the zip file to the default directory (wiod_original_data)
#' \dontrun{getWIOD(dir =  "./wiod_original_data")}
#' 
#' # Downloading the original zip file to the /user/defined/directory.
#' \dontrun{getWIOD("/user/defined/directory")}
#' 
#' @import dplyr
#'
#' @export
getWIOD <- function(dir =  "./wiod_original_data") {

    ## setting the download_dir as a global value
    download.dir <<- dir

    downloadWIOD(download.dir)

    ## creating the list of files
    wiod.files <- list.files(download.dir, pattern="*.RData", full.names = TRUE)

    dir.to.write <<- "./wiod_long_data"
    dir.create(dir.to.write, showWarnings = FALSE)
    
    ## call all functions above with this line, creating a final long file
    ## wiod_long_YEAR.csv to perform network analysis.
    lapply(wiod.files, getLongTables)

}
