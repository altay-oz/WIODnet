#' A function to list yearly, 15 WIOD files obtained after unzip of the
#' orginal downloaded file. All files are in the same directory.
#'
listFiles <- function() {
    wiod.files <- list.files(download_dir, pattern="*.RData", full.names = TRUE)
    return(wiod.files)
}

