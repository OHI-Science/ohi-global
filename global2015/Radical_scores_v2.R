###############################################
## Creating another data document for Radical
## See issue #515 for details
###############################################

#notes for improvement: 
# 1. select data by goals rather than names and then use this to reference the status year information
# 2. a lot of the goals can be combined (maybe the can all be combined; ones with year but no 3rd component; ones with year but with 3rd component)


## data needs to be in this order: component_id, component_name, goal, dimension, scenario, region_id, value, units, source
layers <- read.csv('global2015/layers_2015.csv') %>%
  filter(radical_include=="yes") %>%
  select(component_id, component_name, goal, dimension, scenario, units, source)

summary(layers)

scenario <- "eez2015"

## Note: the layers for the following goals were not added due to formatting that does not lend itself to the data playground
# FIS: There are an undefined number of fish records for each region and most regions have a lot of records
# LE/LIV/ECO: The data that are fed into the toolbox are reported by country name rather than region id
# MAR: There are an undefined number of mariculture records for each region (however, there might be some intermediate data that we can use for this)
# ICO: There are an undefined number of iconic species records for each region and most regions have a lot of records
# SPP: The layers are the trend/status (calculated outside the toolbox); consequently, this information only duplicates the results
# tr_travelwarnings (part of the TR goal): These data are reported for the country rather than the region; the other TR layers are provided

#-----------------------------------------------------------------------------------
### do most of the pressure and resilience layers (these are mostly straightforward)
#-----------------------------------------------------------------------------------

pres_res <- layers %>%
  filter(dimension %in% c("pressure", "resilience")) %>%
  select(component_id, goal, dimension)

p_components <- pres_res$component_id
p_components <- p_components[! (p_components %in% c('cp_habitat_extent_rank', 'cs_habitat_extent', 'hab_presence'))] #these have habitat information

radical <- data.frame()

for(comp in p_components) { # comp="cp_habitat_extent_rank"
  data <- read.csv(sprintf('%s/layers/%s.csv', scenario, comp))
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(names(data) != "region_id")] <- "value"
  data$component_id = comp
  data$component_name = layers$component_name[layers$component_id == comp]
  
radical <- rbind(radical, data)  
}


#-----------------------------------------------------------------------------------
### habitat layers
#-----------------------------------------------------------------------------------

hab_components <- c('cp_habitat_extent_rank', 'cs_habitat_extent', 'hab_presence',
                    'hab_extent', 'hab_health', 'hab_trend')

for(comp in hab_components) { # comp="cp_habitat_extent_rank"
  data <- read.csv(sprintf('%s/layers/%s.csv', scenario, comp))
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(!(names(data) %in% c("region_id", "habitat")))] <- "value"
  data$component_id = comp
  data$component_name = paste0(layers$component_name[layers$component_id == comp], ": ", data$habitat)  
  
  data <- data %>%
    select(region_id, value, component_id, component_name)
  
  radical <- rbind(radical, data)  
}


length(table(radical$component_id))
#-----------------------------------------------------------------------------------
### Easy goal layers
#-----------------------------------------------------------------------------------

status_components <- c('cw_coastalpopn_trend', 'cw_fertilizer_trend', 'cw_pathogen_trend', 'cw_pesticide_trend',
                       'fp_wildcaught_weight', 
                       'np_blast', 'np_cyanide') 

for(comp in status_components) { # comp="ao_access"
  data <- read.csv(sprintf('%s/layers/%s.csv', scenario, comp))
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(names(data) != "region_id")] <- "value"
  data$component_id = comp
  data$component_name = layers$component_name[layers$component_id == comp]
  
  radical <- rbind(radical, data)  
}

length(table(radical$component_id))
#-----------------------------------------------------------------------------------
### NP goal
#-----------------------------------------------------------------------------------

np_components <- c('np_harvest_product_weight', 'np_harvest_tonnes', 'np_harvest_tonnes_relative')

for(comp in np_components) { # comp='np_harvest_tonnes_relative'

  data <- read.csv(sprintf('%s/layers/%s.csv', scenario, comp))
  
if(sum(grepl("year", names(data)))>0){
    f <-  read.csv(sprintf('%s/conf/goals.csv', scenario)) %>%
      select(goal, preindex_function)
    f <- f[f$goal == "NP", ]
    year <- as.numeric(gsub("[^\\d]+", "", f$preindex_function, perl=TRUE))
    data <- data[data$year == year, ]
    data <- data %>%
      select(-year)
  }
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(!(names(data) %in% c("region_id", "product")))] <- "value"
  data$component_id = comp
  data$component_name = paste0(layers$component_name[layers$component_id == comp], ": ", data$product)  
  
  data <- data %>%
    select(region_id, value, component_id, component_name)
  
  radical <- rbind(radical, data)  
}

length(table(radical$component_id))

#-----------------------------------------------------------------------------------
### AO goal
#-----------------------------------------------------------------------------------

ao_components <- c('ao_access', 'ao_need')

for(comp in ao_components) { # comp='ao_need'
  
  data <- read.csv(sprintf('%s/layers/%s.csv', scenario, comp))
  
  if(sum(grepl("year", names(data)))>0){    
    f <-  read.csv(sprintf('%s/conf/goals.csv', scenario)) %>%
      select(goal, preindex_function)
    f <- f[f$goal == "AO", ]
    year <- as.numeric(gsub("[^\\d]+", "", f$preindex_function, perl=TRUE))
    data <- data[data$year == year, ]
    data <- data %>%
      select(-year)
  }
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(names(data) != "region_id")] <- "value"
  data$component_id = comp
  data$component_name = layers$component_name[layers$component_id == comp]
  
  radical <- rbind(radical, data)  
}

length(table(radical$component_id))

#-----------------------------------------------------------------------------------
### LSP goal
#-----------------------------------------------------------------------------------

lsp_components <- c('lsp_prot_area_inland1km', 'lsp_prot_area_offshore3nm')

for(comp in lsp_components) { # comp='lsp_prot_area_offshore3nm'
  
  data <- read.csv(sprintf('%s/layers/%s.csv', scenario, comp))
  
  if(sum(grepl("year", names(data)))>0){    
    f <-  read.csv(sprintf('%s/conf/goals.csv', scenario)) %>%
      select(goal, preindex_function)
    f <- f[f$goal == "LSP", ]
    year <- as.numeric(gsub("[^\\d]+", "", f$preindex_function, perl=TRUE))
    data <- data[data$year == year, ]
    data <- data %>%
      select(-year)
  }
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(names(data) != "region_id")] <- "value"
  data$component_id = comp
  data$component_name = layers$component_name[layers$component_id == comp]
  
  radical <- rbind(radical, data)  
}

length(table(radical$component_id))
#-----------------------------------------------------------------------------------
### TR goal
#-----------------------------------------------------------------------------------

tr_components <- c('tr_jobs_total', 'tr_jobs_tourism', 'tr_jobs_pct_tourism', 'tr_sustainability', 'tr_unemployment')

for(comp in tr_components) { # comp='lsp_prot_area_offshore3nm'
  
  data <- read.csv(sprintf('%s/layers/%s.csv', scenario, comp))
  
  if(sum(grepl("year", names(data)))>0){    
    f <-  read.csv(sprintf('%s/conf/goals.csv', scenario)) %>%
      select(goal, preindex_function)
    f <- f[f$goal == "TR", ]
    year <- as.numeric(gsub("[^\\d]+", "", f$preindex_function, perl=TRUE))
    data <- data[data$year == year, ]
    data <- data %>%
      select(-year)
  }
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(names(data) != "region_id")] <- "value"
  data$component_id = comp
  data$component_name = layers$component_name[layers$component_id == comp]
  
  radical <- rbind(radical, data)  
}

length(table(radical$component_id))
 
#-----------------------------------------------------------------------------------
### formating
#-----------------------------------------------------------------------------------

radical_layers <- layers %>%
  select(component_id, goal, dimension, scenario, units, source)

radical_final <- radical %>%
  left_join(radical_layers, by=c('component_id')) %>%
  filter(!is.na(region_id)) %>%
  filter(region_id <= 250) %>%
  filter(region_id != 213) %>%
  select(component_id, component_name, goal, dimension, scenario, region_id, value, units, source)
 
#-----------------------------------------------------------------------------------
### adding trend data
#-----------------------------------------------------------------------------------

trends <-  read.csv(sprintf('%s/scores.csv', scenario)) %>%
  filter(dimension=="trend") %>%
  mutate(component_id = paste0("trend_", goal)) %>%
  mutate(component_name = paste0("trend_", goal)) %>%
  mutate(scenario = "2015") %>%
  mutate(units = NA) %>%
  mutate(source = "toolbox calculation: change in status during past 5 years") %>%
  select(component_id, component_name, goal, dimension, scenario, region_id, value=score, units, source)
  
radical_final <- rbind(radical_final, trends)
  

write.csv(radical_final, sprintf('global2015/radicalv2_%s_%s.csv', scenario, Sys.Date()),
          row.names=FALSE, na="")