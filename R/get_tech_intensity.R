#' Create a data frame for technology intensity.
#'
#' @description Create a data frame of technology intensity as defined
#'     by EUROSATAT for all NACE coded industries in WIOD files.
#'
#' @usage get_tech_intensity()
#'
#' @import dplyr
#'
#' @import magrittr
#'
getTechIntensity <- function(yearly.raw) {

    ## get the industry and RNr code 
    industry.RNr <- yearly.raw %>% select(RNr, IndustryCode) %>% unique

    ## adding 57 to 61 the code Z. this is done to group them later
    ## 57 to 61 are final consumption
    ## we are using/keeping it for network score calculations.
    RNr <- c(57:61)
    IndustryCode <- "Z"
    df <- data.frame(RNr, IndustryCode, stringsAsFactors = FALSE)

    industry.RNr <- rbind(industry.RNr, df)

    ## changing the name of the manufacturing industry ISIC code into
    ## their technology intensity. Then we will aggregate according to
    ## their technology intensity.
    IndustryCode <- c("C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                "C25","C26","C27","C28","C29","C30","C31_C32","C33")
    tech.type <- c("Low Tech","Low Tech","Low Tech",
                "Low Tech","Low Tech",
                "Medium-Low Tech","Medium-High Tech",
                "High Tech","Medium-Low Tech",
                "Medium-Low Tech","Medium-Low Tech",
                "Medium-Low Tech","High Tech",
                "Medium-High Tech","Medium-High Tech",
                "Medium-High Tech","Medium-High Tech",
                "Low Tech","Low Tech")
    man.table <- data.frame(IndustryCode, tech.type, stringsAsFactors = FALSE)

    industry.RNr  <- left_join(industry.RNr, man.table, by = "IndustryCode")

    industry.RNr %<>% mutate(IndustryCode = ifelse(!is.na(tech.type),
                                                   tech.type, IndustryCode)) %>% select(-tech.type)

    names(industry.RNr) <- c("RNr", "NewIndustryCode")

    return(industry.RNr)
}
