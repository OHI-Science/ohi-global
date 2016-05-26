#' Calculate all the pressures score for each (sub)goal.
#'
#' @param layers object \code{\link{Layers}}
#' @param conf object \code{\link{Conf}}
#' @return data.frame containing columns 'region_id' and per subgoal pressures score
#' @import dplyr
#' @import tidyr
#' @export
CalculatePressuresAll = function(layers, conf){
  
  ## get resilience matrix, components, weights, categories, layers
  p_matrix <- conf$pressures_matrix
  p_matrix <- tidyr::gather(p_matrix, layer, m_intensity, 4:ncol(p_matrix)) %>%    # format the resilience matrix so it is a dataframe    
    dplyr::filter(!is.na(m_intensity)) %>%
    dplyr::select(goal, component, layer, m_intensity)
  
  # p_components: make into a data.frame
  p_components <- conf$config$pressures_components
  p_components <- plyr::ldply(p_components)
  names(p_components) <- c('goal', 'layer')
  
  # gamma weighting for social vs. ecological resilience categories
  p_gamma = conf$config$pressures_gamma                                      
  
  # table describing pressure categories and subcategories
  p_categories <- conf$config$pressures_categories
  p_categories <- suppressWarnings(data.frame(p_categories) %>%
    tidyr::gather("category", "subcategory", 1:2) %>%
    unique())
  
  # list of pressure layers from the pressures_matrix
  p_layers = sort(names(conf$pressures_matrix)[!names(conf$pressures_matrix) %in% c('goal','component','component_name')])
  
  
  # error if layer value range is incorrect
  if (!all(subset(layers$meta, layer %in% p_layers, val_0to1, drop=T))){
    stop(sprintf('These pressures layers must range in value from 0 to 1:\n%s',
                 paste(
                   unlist(
                     layers$meta %>%
                       dplyr::filter(layer %in% p_layers & val_0to1==F) %>%
                       dplyr::select(layer)),
                   collapse = ', ')))
  }
  
  # error if matrix multipliers are not between 0 and 3
  if(sum(p_matrix$value>3 | p_matrix$value<1)>1){
      message(sprintf('There are values in pressures_matrix.csv that are > 3 or < 1'))
  }
  
  
  ## setup initial data.frame for column binding results by region
  regions_dataframe = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T) %>%
    dplyr::select(region_id = id_num)
  regions_vector = regions_dataframe[['region_id']]
  
  
  ## create the weighting scheme
  eco_soc_weight <- data.frame(category = c("ecological", "social"),
                               weight = c(p_gamma, 1-p_gamma))
  eco_soc_weight$category <- as.character(eco_soc_weight$category)
  
  
  ### get the regional data layer associated with each resilience data layer:
  p_rgn_layers <- SelectLayersData(layers, layers=p_layers) %>%
    dplyr::filter(id_num %in% regions_vector) %>%
    dplyr::select(region_id = id_num,
                  val_num,
                  layer) %>%
    dplyr::filter(!is.na(val_num))
  
  ## error check: matrix and region data layers include the same resilience factors
  check <- setdiff(p_layers, p_rgn_layers$layer)
  if (length(check) >= 1) {
    message(sprintf('These pressure layers are in the pressures_matrix.csv, but there are no associated data layers:\n%s',
                    paste(check, collapse=', ')))
  }
  
  check <- setdiff(p_rgn_layers$layer, p_layers)
  if (length(check) >= 1) {
    message(sprintf('These pressure layers have data layers, but are not included in the pressures_matrix.csv:\n%s',
                    paste(check, collapse=', ')))
  }
  

  ## further preparation of matrix data for analysis
  p_matrix <- p_matrix %>%
    dplyr::mutate(subcategory = substring(layer, 1, 2)) %>%
    dplyr::group_by(goal, component, subcategory) %>%
    dplyr::mutate(max_subcategory = max(m_intensity)) %>%
    data.frame()
  
  # merge the region data layers and the resilience matrix
  rgn_matrix <- dplyr::left_join(p_matrix, p_rgn_layers, by="layer")
  
  
  ## summarize cumulative pressure for each subcategory 
  ## (first find maximum pressure in each pressure subcategory)
  calc_pressure <- rgn_matrix %>%
    dplyr::mutate(pressure_intensity = m_intensity*val_num) %>%
    dplyr::left_join(p_categories, by="subcategory") %>%
    data.frame()
  
  
  ## separate method for ecological pressures
  calc_pressure_eco <- calc_pressure %>%
    dplyr::filter(category == "ecological") %>%
    dplyr::group_by(goal, component, category, subcategory, max_subcategory, region_id) %>%
    dplyr::summarize(cum_pressure = sum(pressure_intensity, na.rm=TRUE)/3) %>%
    dplyr::mutate(cum_pressure = ifelse(cum_pressure > 1, 1, cum_pressure)) %>%
    data.frame()  
  
  ## separate method for social pressures
  calc_pressure_soc <- calc_pressure %>%
    dplyr::filter(category == "social") %>%
    dplyr::group_by(goal, component, category, subcategory, max_subcategory, region_id) %>%
    dplyr::summarize(cum_pressure = mean(pressure_intensity)) %>%
    data.frame()  
  
  ## combine social and ecological
 calc_pressure <- rbind(calc_pressure_eco, calc_pressure_soc)
      
  ## average of the pressure subcategories (weighted by highest intensity for each region/subcategory)
  calc_pressure <- calc_pressure %>%
    dplyr::group_by(goal, component, category, region_id) %>%
    dplyr::summarize(pressure = weighted.mean(cum_pressure, max_subcategory)) %>%
        data.frame()
  
  ## combine ecological and social pressures, based on gamma
  calc_pressure <- calc_pressure %>%
    dplyr::left_join(eco_soc_weight, by="category") %>%
    dplyr::group_by(goal, component, region_id) %>%
    dplyr::summarize(pressure = weighted.mean(pressure, weight)) %>%
    data.frame()
  
  ## Deal with goals with components
  
  p_component_layers <- SelectLayersData(layers, layers=p_components$layer) %>%
    dplyr::filter(id_num %in% regions_vector) %>%
    dplyr::select(region_id = id_num,
                  component = category,
                  component_wt = val_num,
                  layer) %>%
    dplyr::filter(!is.na(component)) %>%
    dplyr::filter(!is.na(component_wt)) %>%
    dplyr::left_join(p_components, by="layer") %>%
    dplyr::select(region_id, goal, component, component_wt) %>%
    dplyr::mutate(component = as.character(component))
  
  ## data check:  Make sure components for each goal are included in the resilience_matrix.R
  check <- setdiff(paste(p_component_layers$goal, p_component_layers$component, sep= "-"),
                   paste(p_matrix$goal[p_matrix$goal %in% p_components$goal], p_matrix$component[p_matrix$goal %in% p_components$goal], sep= "-"))
  if (length(check) >= 1) {
    message(sprintf('These goal-components are in the weighting data layers, but not included in the pressure_matrix.csv:\n%s',
                    paste(check, collapse=', ')))
  }
  
  check <- setdiff(paste(p_matrix$goal[p_matrix$goal %in% p_components$goal], p_matrix$component[p_matrix$goal %in% p_components$goal], sep= "-"),
                   paste(p_component_layers$goal, p_component_layers$component, sep= "-"))
  if (length(check) >= 1) {
    message(sprintf('These goal-components are in the pressure_matrix.csv, but not included in the weighting data layers:\n%s',
                    paste(check, collapse=', ')))
  }
  
  ## A weighted average of the components:
  calc_pressure <- calc_pressure %>%
    dplyr::left_join(p_component_layers, by=c('region_id', 'goal', 'component')) %>%
#    dplyr::filter(!(is.na(component_wt) & goal %in% p_components$goal))  %>%
    dplyr::filter(!(is.na(component_wt)))  %>%
    dplyr::mutate(component_wt = ifelse(is.na(component_wt), 1, component_wt)) %>%
    dplyr::group_by(goal, region_id) %>%
    dplyr::summarize(val_num = weighted.mean(pressure, component_wt)) %>%
    data.frame()
  
  # return scores
  scores <- regions_dataframe %>%
    dplyr::left_join(calc_pressure, by="region_id") %>%
    dplyr::mutate(dimension="pressures") %>%
    select(goal, dimension, region_id, score=val_num) %>%
    mutate(score = round(score*100, 2))
  return(scores)
}  