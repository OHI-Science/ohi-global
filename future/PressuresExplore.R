### calculate pressures all, called by calculate_scores_all.R
#### Next one is PressuresExplore2, which is the CalculatePressuresScore fucntion
#### Next one is PressuresExplore3, which is the CalculatePressuresMatrix which is called when a goal has components 

CalculatePressuresAll(layers, conf, gamma = conf$config$pressures_gamma, 
                      debug)

# Getting regions:
D = reshape::rename(SelectLayersData(layers, layers = conf$config$layer_region_labels, 
                            narrow = T), c(id_num = "region_id"))[, "region_id", 
                                                                  drop = F]
regions = D[["region_id"]]


# obtaining relevant data files:
pm = conf$pressures_matrix
pc = conf$config$pressures_components    # goals with components (i.e., mangrove, seagrass, etc.)
pk = conf$config$pressures_categories   # general categories of pressures: environmental (po (pollution), hd (habitat destruction), fp (fishing pressure), sp (species?), cc (climate change)); social (ss)
p.layers = sort(names(pm)[!names(pm) %in% c("goal", "component", 
                                            "component_name")])  # identifying the names of the actual pressure layers from pressure matrix

### Check that all pressures have values between 0 - 1 
if (!all(subset(layers$meta, layer %in% p.layers, val_0to1, 
                drop = T))) {
  warning("Error: Not all pressures layers range in value from 0 to 1!")
  print(subset(layers$meta, layer %in% p.layers & val_0to1 == 
                 F, c("val_min", "val_max"), drop = F))
  stop("")
}

# getting pressure data based on the names of the pressure layers (p.layers step above) and formatting
d.p = reshape::rename(reshape2::dcast(SelectLayersData(layers, layers = p.layers), 
                   id_num ~ layer, value.var = "val_num"), c(id_num = "region_id"))

d.p = subset(d.p, region_id %in% regions)
nr = length(regions)
np = length(p.layers)
subgoals = subset(conf$goals, !goal %in% unique(conf$goals$parent), 
                  goal, drop = T)      # get rid of goals combined of subgoals

for (g in subgoals) { # g = 'FIS' g='HAB' g='NP'
  if (debug) 
    cat(sprintf("goal: %s\n", g))
  P = w = p = alpha = beta = NA    
  p = matrix(as.matrix(d.p[, -1]), nrow = nr, ncol = np,    # convert pressure data to a matrix
             dimnames = list(region_id = d.p[[1]], pressure = names(d.p)[-1]))
  p.components = pm$component[pm$goal == g]    ## from the pressure matrix, pull out the components for the goal (if there are components)
  if (length(p.components) == 1) {  # if no components, this is run
    if (debug) 
      cat("  no components\n")
    w <- matrix(rep(unlist(pm[pm$goal == g, p.layers]),   # from the pm gets the line for the goal/subgoal and repeats it for each region
                    nr * np), byrow = T, nrow = nr, ncol = np, dimnames = list(region_id = regions, 
                                                                               pressure = p.layers))
    P = CalculatePressuresScore(p, w, pressures_categories = pk, 
                                GAMMA = gamma)
  }
  else {  # if the goal has components, this is run:
    if (debug) 
      cat(" ", length(p.components), "components:", 
          paste(p.components, collapse = ", "), "\n")
    
    ## select all rows from the pressure matrix of that goal
    alpha <- matrix(as.matrix(pm[pm$goal == g, p.layers]), 
                    nrow = length(p.components), ncol = length(p.layers), 
                    dimnames = list(category = p.components, pressure = p.layers))  
    
    stopifnot(g %in% names(pc))   # checks against the list of goals with subcomponents to make sure it is in the list
    stopifnot(pc[[g]][["layer"]] %in% names(layers))  # this makes sure the data layer that has the weights for the components is in the list of layers
    
    d_w = plyr::rename(SelectLayersData(layers, layers = pc[[g]][["layer"]], 
                                  narrow = T), c(id_num = "region_id", val_num = "value"))  # read in the component weight data layer
 
    # check to make sure all the components in the pressure table are in the weight data layer  
    if (!all(p.components %in% unique(d_w$category))) {
      message(sprintf("The following components for %s are not in the aggregation layer %s categories (%s): %s", 
                      g, pc[[g]][["layer"]], paste(unique(d_w$category), 
                                                   collapse = ", "), paste(p.components[!p.components %in% 
                                                                                          d_w$category], collapse = ", ")))
    }
  
      if (pc[[g]][["level"]] == "region_id-category") {  # NP is the only one with this level...not sure what this means exactly...
      if (debug) 
        cat(sprintf("  scoring pressures seperately by region and category, like a subgoal (pressures_calc_level=='region_id-category')\n"))
      if (exists("krp")) 
        rm(krp)
        
        ## loop through each component and calculate pressure for each component/region id (this is the environmental pressures)
      for (k in p.components) { # k = 'corals'
   
        # make a matrix with the weights of the relevant pressures for each region
        w <- matrix(rep(unlist(pm[pm$goal == g & pm$component == 
                                    k, p.layers]), nr * np), byrow = T, nrow = nr, 
                    ncol = np, dimnames = list(region_id = regions, 
                                               pressure = p.layers))
        
        # calculate the pressure score for each region/subcategory
        rp.k = data.frame(category = k, region_id = as.integer(dimnames(p)$region_id), 
                          p = CalculatePressuresScore(p, w, pressures_categories = pk, 
                                                      GAMMA = gamma))
        if (exists("krp")) {
          krp = rbind(krp, rp.k)
        }
        else {
          krp = rp.k
        }
      }
        
        ## join the krp file (pressure for each region/subcomponent) to the region/subcomponent weights (i.e., 90% harvest is coral; 10% sponges, etc.)
      krpw = krp %>% inner_join(d_w, by = c("region_id", 
                                            "category")) %>% arrange(region_id, category) %>% 
        select(region_id, category, p, w = value)
      
      d_region_ids = D[, "region_id", drop = F]
      
      # for each region, take the mean of the pressures, weighted by the proportion harvest
      krpwp = d_region_ids %>% 
        left_join(krpw, by = "region_id") %>% 
        group_by(region_id) %>% 
        summarize(p = sum(w * p)/sum(w))
      
      P = round(krpwp$p, 2)
      names(P) = krpwp$region_id
      }
    
    else if (pc[[g]][["level"]] == "region_id") {
      if (debug) 
        cat(sprintf("  aggregating across categories to region (pressures_calc_level=='region_id')\n"))

      ## not clear to me what this is doing....and where the "regions_countries_areas" is being called      
      if (!is.na(subset(layers$meta, layer == pc[[g]][["layer"]], 
                        fld_id_chr, drop = T))) {
        stop("surprise, layers_data_bycountry used")
        if (debug) 
          cat(sprintf("  using layers_data='layers_data_bycountry'\n"))
        
        
        d_w_r = d_w %>% inner_join(regions_countries_areas, 
                                   by = "country_id") %>% filter(region_id %in% 
                                                                   regions) %>% select(region_id, category, 
                                                                                       country_id, country_area_km2)
        m_w = reshape2::dcast(d_w_r, region_id ~ category, sum)
      }
      else {   # this is actually what happens for the region_id
        if (debug) 
          cat(sprintf("  using layers_data='layers_data'\n"))
        
        # identify/format which habitats are present in which regions:
        m_w = reshape2::dcast(subset(d_w, region_id %in% regions), 
                    region_id ~ category, sum, margins = c("category"))
        
        #calculate proportion of total habitats available in each region
        m_w = cbind(m_w[, "region_id", drop = F], m_w[, 
                                                      2:(ncol(m_w) - 1)]/m_w[, "(all)"])
      }
      
      beta = matrix(as.matrix(m_w[, -1]), nrow = nrow(m_w), 
                    ncol = ncol(m_w) - 1, dimnames = list(region_id = m_w$region_id, 
                                                          category = names(m_w)[-1]))
      beta = beta[, intersect(rownames(alpha), colnames(beta)), 
                  drop = F]
      if (debug) 
        cat(sprintf("  CalculatePressuresMatrix(alpha, beta, calc='avg')\n"))
      w = CalculatePressuresMatrix(alpha, beta, calc = "avg")
      region_ids.missing = setdiff(regions, dimnames(w)$region_id)
      pressures.missing = setdiff(p.layers, dimnames(w)$pressure)
      w = matrix(rbind(cbind(w, matrix(0, nrow = nrow(w), 
                                       ncol = length(pressures.missing))), matrix(0, 
                                                                                  nrow = length(region_ids.missing), ncol = ncol(w) + 
                                                                                    length(pressures.missing))), nrow = nrow(w) + 
                   length(region_ids.missing), ncol = ncol(w) + 
                   length(pressures.missing), dimnames = list(region_id = c(dimnames(w)$region_id, 
                                                                            region_ids.missing), pressure = c(dimnames(w)$pressure, 
                                                                                                              pressures.missing)))[as.character(regions), 
                                                                                                                                   p.layers, drop = F]
      w = w[dimnames(p)$region_id, , drop = F]
      stopifnot(all(dimnames(w)$pressure == dimnames(w)$pressure))
      stopifnot(!is.null(dimnames(w)$region_id))
      stopifnot(all(dimnames(p)$region_id == dimnames(w)$region_id))
      P = CalculatePressuresScore(p, w, pressures_categories = pk, 
                                  GAMMA = gamma)
    }
    else {
      stop(sprintf("pressures_component_aggregation.csv : pressures_calc_level of '%s' not handled. Must be either 'region_id' or 'region_id-category'.", 
                   agg$aggregation_sequence))
    }
  }
  D = merge(D, setNames(data.frame(names(P), P), c("region_id", 
                                                   g)), all.x = T)
}
scores = cbind(melt(D, id.vars = "region_id", variable.name = "goal", 
                    value.name = "score"), dimension = "pressures")
head(scores)
return(scores)