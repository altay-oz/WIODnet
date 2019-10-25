#' Generate yearly long tables for network analysis.
#'
#' @description Generate a long network file with source, target and
#'     weight for each country and economic sector represented with
#'     country code in three character and industry.
#'
#' @param yearly.IO network matrix among industries obtained from the raw data.
#' 
#' @return data frame
#'
#' @import dplyr
#'
#' @importFrom tidyr unite
#'
#' @importFrom tidyr separate
#'
#' @importFrom tidyr gather
#'
#' @importFrom magrittr extract
#'
getNetLong <- function(yearly.IO, industry.RNr) {

    yearly.IO %<>% select(-c("Year", "IndustryDescription", "RNr", "TOT"))

    ## joining Country and IndustryCode columns to create source (country.ind)
    yearly.IO %<>% tidyr::unite("source", "Country", "IndustryCode", sep = ".") 

    ## creating the long table
    yearly.IO <- tidyr::gather(yearly.IO, target.country.ind,
                        raw.weight, 2:ncol(yearly.IO), factor_key = FALSE)

    ## giving the industry code to the target.country.ind column
    yearly.IO %<>% tidyr::separate(target.country.ind, c("target.country", "target.ind"), 3)

    yearly.IO$target.ind <- as.numeric(yearly.IO$target.ind)
    
    yearly.IO <- left_join(yearly.IO, industry.RNr, by=c("target.ind" = "RNr")) 

    yearly.IO %<>% select(-target.ind)

    ## removing the final consumption
    yearly.IO %<>% filter("NewIndustryCode" != "Z")
    
    yearly.IO %<>% tidyr::unite("target", "target.country", "NewIndustryCode", sep = ".")

    ## ## aggregating according the Low Tech etc in target and source
    ## ## dividing the target column into country|ind|z.cat and making their sum
    yearly.IO %<>% group_by(source, target) %>% summarise(weight = sum(raw.weight)) %>% as.data.frame

    return(yearly.IO)
}
