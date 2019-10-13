#' Generate a long table for VA, TXSP etc and other complementary values.
#'
#' @description Generate yearly long tables for six yearly different
#'     values for each economic sectors. These values are total
#'     intermediate consumption (II_fob), taxes less subsidies on
#'     products (TXSP), Cif/ fob adjustments on exports (EXP_adj),
#'     direct purchases abroad by residents (PURR), purchases on the
#'     domestic territory by non-residents (PURNR), value added at basic
#'     prices (VA).
#'
#' @param yearly.complementary the last portion of the yearly WIOD data
#'     containing complementary values.
#'
#' @param value one of the six different values; VA, II_fob, TXSP, EXP_adj, PURR, RURNR.
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
getComplementary  <- function(yearly.complementary, value) {

    yearly.select <- yearly.complementary %>% filter(IndustryCode == value) %>%
        select(6:(ncol(yearly.complementary) - 1))
            ##        select(AUS1:ROW61)

    yearly.select.long <- tidyr::gather(yearly.select, countryind, factor_key = FALSE)

    ## repetition function which takes columnname, value to be summed and name
    yearly.select.long %<>% tidyr::separate(countryind, c("country", "ind"), 3)

    yearly.select.long$ind <- as.numeric(yearly.select.long$ind)

    yearly.select.long <- left_join(yearly.select.long, industry.RNr, by=c("ind" = "RNr")) 

    head(yearly.select.long)

    yearly.select.long %<>% select(-ind)

    yearly.select.long %<>% tidyr::unite("country.ind", "country", "NewIndustryCode", sep = ".")

    ## change the VA to the variable "value" as column name.
    yearly.select.long %<>% group_by(country.ind) %>% summarise(sum = sum(value))

    names(yearly.select.long) <- c("country.ind", value)

    return(yearly.select.long)    
}
