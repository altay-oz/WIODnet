#' Generate a long table for domestic and international trade
#'
#' @description Generate yearly long tables for final and itermediate,
#'     in and out, domestic and internatioanl trade values for each
#'     economic sectors.
#'
#' @param net.long yearly long table of source target weight network data
#' 
#' @return data frame
#' 
#' @import dplyr
#'
#' @import magrittr
#' 
domIntTrade <- function(net.long) {
    ## inserting a long table of network realtion (source/target/weight)
    ## and obtaining the weight for each node's (country.ind)
    ## international and domestic trade.

    ## obtaining all nodes for making a left join at the end
    source <- net.long %>% select(source) %>% unique
    target <- net.long %>% select(target) %>% unique

    names(source) <- "country.ind"
    names(target) <- "country.ind"

    all.nodes <- rbind(source, target) %>% unique
    
    ## adding two columns source.country and target.country
    net.long %<>% mutate(source.country=substr(source, 1,3)) %>% 
        mutate(target.country=substr(target, 1,3))

    ## adding two columns source.industry and target.industry
    net.long %<>% mutate(source.industry=substr(source, 5, 25)) %>% 
        mutate(target.industry=substr(target, 5, 25))

    #####################
    ## domestic and international in and out weight for each
    ## country.industry (source)
    
    ## summing the the weights according to target country domestic
    dom.out <- net.long %>% filter(source.country == target.country) %>%
        group_by(source) %>% summarise(dom.out.weight = sum(weight)) 
    names(dom.out)[1] <- "country.ind"
    
    ## summing the the weights according to target country international
    int.out <- net.long %>% filter(source.country != target.country) %>%
        group_by(source) %>% summarise(int.out.weight = sum(weight)) 
    names(int.out)[1] <- "country.ind"
    
    ## summing the the weights according to target country domestic
    dom.in <- net.long %>% filter(source.country == target.country) %>%
        group_by(target) %>% summarise(dom.in.weight = sum(weight)) 
    names(dom.in)[1] <- "country.ind"
    
    ## summing the the weights according to target country international
    int.in <- net.long %>% filter(source.country != target.country) %>%
        group_by(target) %>% summarise(int.in.weight = sum(weight)) 
    names(int.in)[1] <- "country.ind"
    
    #####################
    ## domestic and international out weight for final consumption (Z)
    ## for each country.industry (source)
    
    ## domestic final consumption
    dom.Z <- net.long %>% filter(source.country == target.country, target.industry == "Z") %>%
        group_by(source) %>% summarise(dom.Z.weight = sum(weight))
    names(dom.Z)[1] <- "country.ind"

    ## international final consumption
    int.Z <- net.long %>% filter(source.country != target.country, target.industry == "Z") %>%
        group_by(source) %>% summarise(int.Z.weight = sum(weight))
    names(int.Z)[1] <- "country.ind"

    ## merge all
    dom.int.weights <- all.nodes %>% left_join(dom.out, by = "country.ind") %>%
        left_join(int.out, by = "country.ind") %>% left_join(dom.in, by = "country.ind") %>%
        left_join(int.in, by = "country.ind") %>% left_join(dom.Z, by = "country.ind") %>%
        left_join(int.Z, by = "country.ind")

    return(dom.int.weights)
}

