#' A function to donwload WIOD zip file from the project web page.  The
#' downloaded file is the 2016 version which contains RData for each
#' year for the period 2000:2014. The integrity of the downloaded file
#' is checked with md5sum.
#'
#' @usage get_wiod(download_dir, data_URL)
#'
#' @param download_dir The directory into which the zip file is downloaded
#'
#' @param data_URL The URL adress of the original WIOD data
#'
#' @examples
#' # Download to the default directory (wiod_original_data) and from
#' # the default URL.
#'
#' # Download to the user defined directory
#' 
#' @importFrom utils download.file
#'
#' @importFrom tools md5sum
#' 
get_wiod <- function(download_dir = "./wiod_orginal_data",
                     data_URL = "http://www.wiod.org/protected3/data16/wiot_ROW/wiot_r_Nov16.zip") {

    ## create the directory to download
    dir.create(download_dir, showWarnings = FALSE)

    ## set the directory and the file name to download 
    original_file  <- paste0(download_dir, "/wiot_r_Nov16.zip")

    ## check if the downloaded file exist, if not start download
    if (!file.exists(original_file)) {
        res <- tryCatch(download.file(data_URL,
                                      destfile = original_file,
                                      method = "auto"),
                        error=function(e) 1)

        ## check the integrity of the downloaded file
        check_wiod(original_file)
        
    } else {
        ## check the integrity of the allready downloaded file
        check_wiod(original_file)
    }
}

check_wiod <- function(original_file) {
    
    ## obtained from the downloaded file
    md5sum_wiod <- "8d313ac0c9f113e16ac66e8c7fe5bf51"

    if (! identical(as.vector(md5sum(original_file)), md5sum_wiod)) {
        message("md5sum is NOT good.")
    } else {
        message("md5sum is good.")
    }
}


skip_if_no_download <- function() {
    if(TRUE) {
        skip("No download is possible or download is too long.")
    }
}

