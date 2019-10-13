#' Join the tech intensity data frame to the yearly WIOD
#'
#' @description Join the created technology intensity data frame to the
#'     yearly raw WIOD wide table so that similar country and technology
#'     intensity manufacturing industies can be aggregated.
#'
#' @param yearly.raw yearly raw data from the downloaded zip such as WIOT2011_October16_ROW.RData
#' 
#' @return data frame
#'
#' @import dplyr
#'
#' @import magrittr
#' 
addTechIntensity <- function(yearly.raw) {

    industry.RNr <<- getTechIntensity(yearly.raw)
    
    ## changing the IndustryCode column wihtin the main df
    yearly.raw %<>% left_join(industry.RNr, yearly.raw, by = c("RNr"))
    
    ## cleaning the data frame
    yearly.raw %<>% select(-IndustryCode) %>% rename(IndustryCode = NewIndustryCode)

    return(yearly.raw)
}
