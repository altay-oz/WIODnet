#' Download the WIOD zip file from its web site.
#'
#' @description Download WIOD zip file from the project web page. The
#'     downloaded file is the 2016 version which contains RData for each
#'     year for the period 2000:2014. The integrity of the downloaded
#'     file is checked with md5sum if it passes then it is unziped.
#'
#' @param download.dir directory to download the original WIOD data. 
#'
#' @import utils
#'
#' @importFrom tools md5sum
#' 
downloadWIOD <- function(download.dir) {
    data.URL  <-  "http://www.wiod.org/protected3/data16/wiot_ROW/wiot_r_Nov16.zip"

    ## create the directory to download
    dir.create(download.dir, showWarnings = FALSE)

    ## set the directory and the file name to download 
    original.file  <- paste0(download.dir, "/wiot_r_Nov16.zip")

    ## check if the downloaded file exist, if not start download
    if (!file.exists(original.file)) {
        res <- tryCatch(download.file(data_URL,
                                      destfile = original.file,
                                      method = "auto"),
                        error=function(e) 1)

        ## check the integrity of the downloaded file
        checkWIOD(original.file)
        
    } else {
        ## check the integrity of the allready downloaded file
        checkWIOD(original.file)
    }
}

checkWIOD <- function(original.file) {
    
    ## obtained from the downloaded file
    md5sum.wiod <- "8d313ac0c9f113e16ac66e8c7fe5bf51"

    if (! identical(as.vector(md5sum(original.file)), md5sum.wiod)) {
        stop("md5sum is NOT good.")
    } else {
        message("md5sum is good.")
        ## downloaded file is legit, now extracting to the director
        ## where it is downloaded
        unzip(original.file, exdir=download.dir)
    }
}

skip_if_no_download <- function() {
    if(TRUE) {
        skip("No download is possible or download is too long.")
    }
}
