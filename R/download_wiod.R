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
downloadWIOD <- function(download.dir) {
    data.URL  <-  "http://www.wiod.org/protected3/data16/wiot_ROW/wiot_r_Nov16.zip"

    ## create the directory to download
    dir.create(download.dir, showWarnings = FALSE)

    ## set the directory and the file name to download 
    original.file  <- paste0(download.dir, "/wiot_r_Nov16.zip")

    ## check if the downloaded file exist, if not start download
    if (!file.exists(original.file)) {
        res <- tryCatch(download.file(data.URL,
                                      destfile = original.file,
                                      method = "auto"),
                        error=function(e) 1)

        ## check the integrity of the downloaded file
        checkWIOD(original.file, download.dir)
        
    } else {
        ## check the integrity of the allready downloaded file
        checkWIOD(original.file, download.dir)
    }
}

#' Check if the downloaded file is the one downloaded by Altay.
#'
#' @description Check the integrity of the zip file with the md5sum
#'     obtained by Altay. The md5sum is not provided by the WIOD source.
#'
#' @param original.file the zip file downloaded.
#'
#' @importFrom tools md5sum
#' 
checkWIOD <- function(original.file, download.dir) {
    
    ## obtained from the downloaded file
    md5sum.wiod <- "8d313ac0c9f113e16ac66e8c7fe5bf51"

    if (! identical(as.vector(md5sum(original.file)), md5sum.wiod)) {
        stop("md5sum of the downloaded file is NOT good.")
    } else {
        message("md5sum of the dowloaded is good, unzipping and continuing on creating long files.")
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
