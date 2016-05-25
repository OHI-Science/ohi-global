#' Calculate all the resilience score for each (sub)goal.
#'
#' @param layers object \code{\link{Layers}}
#' @param conf object \code{\link{Conf}}
#' @return data.frame containing columns 'region_id' and per subgoal resilience score
#' @import tidyr
#' @import dplyr
#' @export
CalculateResilienceAll = function(layers, conf, debug=FALSE){
  
    ## error unless layer value range is correct
    if (!all(subset(layers$meta, layer %in% r.layers, val_0to1, drop=T))){
      stop(sprintf('These resilience layers do not range in value from 0 to 1:\n%s',
                   paste(
                     unlist(
                       layers$meta %>%
                         filter(layer %in% r.layers & val_0to1==F) %>%
                         select(layer)),
                     collapse = ', ')))
    }
    
    
    ## get resilience matrix, components, weights, categories, layers
    rc = conf$config$resilience_components                                 # weighting data for goals with components
    rc = plyr::ldply(resilience_components)
    names(rc) <- c('goal', 'layer')
    
    rg = conf$config$resilience_gamma                                      # gamma weighting for social vs. ecological resilience categories
    
    rm = conf$resilience_matrix
    rm = within(rm, {component[is.na(component)] = ''})                    # resilience matrix  
    
    rw = conf$resilience_weights                                           # resilience weights table
    
    r.layers = setdiff(names(rm), c('goal','component','component_name'))  # list of resilience layers from matrix
    
    ## setup initial data.frame for column binding results by region
    D = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T) %>%
      dplyr::select(region_id = id_num)
    regions = D[['region_id']]
    
    ## create the weighting scheme
    eco_soc_weighting <- data.frame(category = c("ecological", "social"),
                                    weight = c(rg, 1-rg))
    eco_soc_weighting$category <- as.character(eco_soc_weighting$category) 
    
    
    ### get the regional data for each resilience layer:
    r_tmp <- SelectLayersData(layers, layers=r.layers) %>%
      dplyr::filter(id_num %in% regions) %>%
      dplyr::select(region_id = id_num,
                    val_num, 
                    layer) %>%
      dplyr::filter(!is.na(val_num))
    
    
    ### format the resilience matrix so it is a dataframe    
    rm_tmp <- tidyr::gather(rm, layer, included, 3:ncol(rm)) %>%
      dplyr::filter(!is.na(included)) %>%
      dplyr::select(goal, component, layer)
    
    
    ## error check: matrix and data layers include the same resilience factors
    setdiff(r.layers, r_tmp$layer)
    setdiff(r_tmp$layer, r.layers)
    
    test2 <- dplyr::left_join(rm_tmp, r_tmp, by="layer")
    
    ## check that matrix and weights table include the same resilience factors
    setdiff(r.layers, rw$layer)
    setdiff(rw$layer, r.layers)
    
    test2 <- dplyr::left_join(test2, rw, by="layer")
    
    ## average subcategories of resilience layers
    test3 <- test2 %>%
      dplyr::group_by(goal, component, region_id, category, category_type, subcategory) %>%
      dplyr::summarize(max_subcategory = max(weight),
                       val_num = weighted.mean(val_num, weight)) %>%
      data.frame()
    
    ## average category types of resilience layers (weight by max weight in each subcategory)
    test4 <- test3 %>%
      dplyr::group_by(goal, component, region_id, category, category_type) %>%
      dplyr::summarize(val_num = weighted.mean(val_num, max_subcategory)) %>%
      data.frame()
    
    ## average ecological components (ecosystem and regulatory)
    test5 <- test4 %>%
      dplyr::group_by(goal, component, region_id, category) %>%
      dplyr::summarize(val_num = mean(val_num)) %>%
      data.frame()
    
    
    ## combine ecological and social based on resilience gamma weighting
    test6 <- test5 %>%
      dplyr::left_join(eco_soc_weighting, by="category") %>%
      dplyr::group_by(goal, component, region_id) %>%
      dplyr::summarise(val_num = weighted.mean(val_num, weight)) %>%
      data.frame()
    
    ## For goals with components, get the relevant data layers used for weights
    components <- SelectLayersData(layers, layers=rc$layer) %>%
      dplyr::filter(id_num %in% regions) %>%
      dplyr::select(region_id = id_num,
                    component = category, 
                    component_wt = val_num, 
                    layer) %>%
      dplyr::filter(!is.na(component)) %>%
      dplyr::filter(!is.na(component_wt)) %>%
      dplyr::left_join(rc, by="layer") %>%
      dplyr::select(region_id, goal, component, component_wt) %>%
      dplyr::mutate(component = as.character(component))
    
    
    ## A weighted average of the components:
    test7 <- test6 %>%
      dplyr::left_join(components, by=c('region_id', 'goal', 'component')) %>%
      dplyr::filter(!(is.na(component_wt) & goal %in% rc$goal))  %>%
      dplyr::mutate(component_wt = ifelse(is.na(component_wt), 1, component_wt)) %>%
      dplyr::group_by(goal, region_id) %>%
      dplyr::summarize(val_num = weighted.mean(val_num, component_wt)) 
    
    # return scores    
    scores <- D %>%
      dplyr::left_join(test7, by="region_id") %>%
      dplyr::mutate(dimension="resilience") %>%
      select(goal, dimension, region_id, score=val_num) %>%
      mutate(score = round(score*100, 2))
    return(scores)
    
}
    