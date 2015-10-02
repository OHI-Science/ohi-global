if(radical2 == TRUE){

###############################################
## Creating another data document for Radical
## See issue #515 for details
###############################################

# library(dplyr)
# library(tidyr)
# setwd("~/ohi-global")

# scenario <- "2015"

## data needs to be in this order: component_id, component_name, goal, dimension, scenario, region_id, value, units, source
layers <- read.csv(sprintf('%s/layers_2015.csv', saveFile)) %>%
  filter(radical_include=="yes") %>%
  select(component_id, component_name, component_label, goal, dimension, scenario, units, source, url)
table(layers$goal) 


## Note: the layers for the following goals were not added due to formatting that does not lend itself to the data playground
# FIS: There are an undefined number of fish records for each region and most regions have a lot of records
# LE/LIV/ECO: The data that are fed into the toolbox are reported by country name rather than region id
# MAR: There are an undefined number of mariculture records for each region (however, there might be some intermediate data that we can use for this)
# ICO: There are an undefined number of iconic species records for each region and most regions have a lot of records
# SPP: The layers are the trend/status (calculated outside the toolbox); consequently, this information only duplicates the results
# tr_travelwarnings (part of the TR goal): These data are reported for the country rather than the region; the other TR layers are provided

# BD and SP are calculated from subgoals

##########################################
## Getting the combinations for pressures/resilience and goals
## This part of the script ammends the layers file so that each pressure/resilience
## includes its associated goals
#####################################################
## get all the goals that each pressure applies too:
pressures <- layers %>%
  filter(goal=="pressures") %>%
  select(-goal)

pMatrix <- read.csv(sprintf('eez%s/conf/pressures_matrix.csv', scenario))

pMatrix <- gather(pMatrix, "component_id", "weight", 4:ncol(pMatrix)) %>%
  group_by(goal, component_id) %>%
  summarize(weight = sum(weight, na.rm=TRUE)) %>%
  filter(weight > 0) %>%
  select(goal, component_id) %>%
  arrange(component_id) %>%
  ungroup() 

#check
filter(pMatrix, goal=='CW')
filter(pMatrix, goal=='CP')

pData <- pMatrix %>%
  left_join(pressures)

# add to the layers document:
layers <- layers %>%
  filter(goal != "pressures") %>%
  bind_rows(pData)

#######################################################
## get all the goals that each resilience applies too:
res <- layers %>%
  filter(goal=="resilience") %>%
  select(-goal)

rMatrix <- read.csv(sprintf('eez%s/conf/resilience_matrix.csv', scenario))

rMatrix <- gather(rMatrix, "component_id", "weight", 3:ncol(rMatrix)) %>%
  filter(weight != "") %>%
  select(goal, component_id) %>%
  unique() %>%
  arrange(component_id)

## data check
filter(rMatrix, goal=="CW")
filter(rMatrix, goal=="CP")

rData <- rMatrix %>%
  left_join(res)

# add to the layers document:
layers <- layers %>%
  filter(goal != "resilience") %>%
  bind_rows(rData)

summary(layers)
table(layers$goal)


#########################################################################
### do most of the pressure and resilience layers (these are mostly straightforward)
## Habitat ones are excluded because they include an extra variable that has to be
## controlled for
#########################################################################

### blank pressures/resilience dataframe to add data
p_r_radical <- data.frame()

pres_res_layers <- layers %>%
  filter(dimension %in% c("pressures", "resilience")) 

p_components <- unique(pres_res_layers$component_id)
p_components <- p_components[! (p_components %in% c('cp_habitat_extent_rank', 'cs_habitat_extent', 'hab_presence'))] #these have habitat information

pres_res_data <- data.frame()

for(comp in p_components) { # comp="wgi_all"
  data <- read.csv(sprintf('eez%s/layers/%s.csv', scenario, comp))
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(names(data) != "region_id")] <- "value"
  data$component_id = comp
  
pres_res_data <- rbind(pres_res_data, data)  
}

pres_res_data <- pres_res_data %>%
  left_join(pres_res_layers, by=c('component_id')) %>%
  mutate(subcomponent_id = NA)

#data check ##
filter(pres_res_data, goal=='CW', region_id==1) %>%
  select(-source)
filter(pres_res_data, goal=='AO', region_id==1) %>%
  select(-source)
#end: data check ##

## combine with radical data
p_r_radical <- rbind(p_r_radical, pres_res_data)

#data check##
p_r_radical %>%
  filter(goal == "CP")%>%
  filter(region_id == 163) %>%
  select(-source)
#end: data check ##

############################################################
## goal data
############################################################

#initialize the goal dataframe
s_t_radical <- data.frame()

stat_trend_layers <- layers %>%
  filter(dimension %in% c("status", "trend")) 

## these are pressures but have additional habitat component
extras <- layers %>%
  filter(component_id %in% c('cp_habitat_extent_rank', 'cs_habitat_extent'))

stat_trend_layers <- stat_trend_layers %>%
  bind_rows(extras)

#-----------------------------------------------------------------------------------
### Goal layers with no subcomponents
#-----------------------------------------------------------------------------------

goals_sub <- c('CW', 'FP', 'AO', 'LSP', 'TR', 'NP')

for(goal in goals_sub){ # goal='NP'
  
status_components <- stat_trend_layers$component_id[stat_trend_layers$goal == goal]


status_components <- status_components[! status_components %in%  c("np_harvest_product_weight", "np_harvest_tonnes", "np_harvest_tonnes_relative")]

s_t_radical_comp <- data.frame()

for(comp in status_components) { # comp="np_blast"
data <- read.csv(sprintf('eez%s/layers/%s.csv', scenario, comp))  
  
if(sum(grepl("year", names(data)))>0){    
  f <-  read.csv(sprintf('eez%s/conf/goals.csv', scenario)) %>%
    select(goal, preindex_function)
  f <- f[f$goal == goal, ]
  year <- as.numeric(gsub("[^\\d]+", "", f$preindex_function, perl=TRUE))
  data <- data[data$year == year, ]
  data <- data %>%
    select(-year)
}

names(data)[which(names(data)=="rgn_id")] <- "region_id"
names(data)[which(names(data) != "region_id")] <- "value"
data$component_id = comp
data$component_name = unique(layers$component_name[layers$component_id == comp])
data$component_label = unique(layers$component_label[layers$component_id == comp])
data$goal = goal
data$url = unique(layers$url[layers$component_id == comp])
data$subcomponent_id = NA

s_t_radical_comp <- rbind(s_t_radical_comp, data)

}
s_t_radical <- rbind(s_t_radical, s_t_radical_comp)

}

#-----------------------------------------------------------------------------------
### NP goals: years and subcomponents
#-----------------------------------------------------------------------------------

np_components <- stat_trend_layers$component_id[stat_trend_layers$goal == "NP"] 
np_components <- np_components[! (np_components %in% c('np_blast', 'np_cyanide'))] # taken care of above

for(comp in np_components) { # comp='np_harvest_tonnes_relative'

  data <- read.csv(sprintf('eez%s/layers/%s.csv', scenario, comp))
  
if(sum(grepl("year", names(data)))>0){
    f <-  read.csv(sprintf('eez%s/conf/goals.csv', scenario)) %>%
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
  data$component_label = paste0(unique(layers$component_label[layers$component_id == comp]), 
                                ": ", data$product)
  data$subcomponent_id = data$product
  data$goal = 'NP'
  data$url = unique(layers$url[layers$component_id == comp])
  
  
    
  data <- data %>%
    select(region_id, value, component_id, component_name, component_label, goal, url, subcomponent_id)
  
  s_t_radical <- rbind(s_t_radical, data)    
}

# > dim(s_t_radical)
# [1] 5411    8

#-----------------------------------------------------------------------------------
### habitat layers (includes some pressure data as well)
#-----------------------------------------------------------------------------------

goals_sub <- c('HAB')
hab_components <- unique(stat_trend_layers$component_id[stat_trend_layers$goal %in% goals_sub]) 
  

for(comp in hab_components) { # comp="hab_extent"
  data <- read.csv(sprintf('eez%s/layers/%s.csv', scenario, comp)) %>%
  filter(habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom'))
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(!(names(data) %in% c("region_id", "habitat")))] <- "value"
  data$component_id = comp
  data$component_name = paste0(layers$component_name[layers$component_id == comp], ": ", data$habitat)  
  data$component_label = paste0(unique(layers$component_label[layers$component_id == comp & layers$goal == 'HAB']),
                                ": ", 
                                data$habitat)
  data$goal = 'HAB'
  data$url = unique(layers$url[layers$component_id == comp])
  data$subcomponent_id = data$habitat
  
    
  data <- data %>%
    select(region_id, value, component_id, component_name, component_label, goal, url, subcomponent_id) %>%
    unique()
  
  s_t_radical <- rbind(s_t_radical, data)    

}

# > dim(s_t_radical)
# [1] 7222    8

goals_sub <- c('CP')
hab_components <- unique(stat_trend_layers$component_id[stat_trend_layers$goal %in% goals_sub]) 


for(comp in hab_components) { # comp="hab_extent"
  
  if(grepl('extent', comp)){
  data <- read.csv(sprintf('eez%s/layers/%s.csv', scenario, comp)) %>%
    filter(habitat %in% c('coral','mangrove_inland1km','mangrove_offshore', 'saltmarsh','seaice_edge','seagrass'))
  } else {
    data <- read.csv(sprintf('eez%s/layers/%s.csv', scenario, comp)) %>%
      filter(habitat %in% c('coral','mangrove', 'saltmarsh','seaice_edge','seagrass'))
  }
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(!(names(data) %in% c("region_id", "habitat")))] <- "value"
  data$component_id = comp
  data$component_name = paste0(layers$component_name[layers$component_id == comp], ": ", data$habitat)  
  data$goal = 'CP'
  data$subcomponent_id = data$habitat
  data$component_label = paste0(unique(layers$component_label[layers$component_id == comp & layers$goal=="CP"]), ": ",
                               data$habitat)
  data$url = unique(layers$url[layers$component_id == comp])
  

  data <- data %>%
    select(region_id, value, component_id, component_name, component_label, goal, url, subcomponent_id) %>%
    unique()
  
  s_t_radical <- rbind(s_t_radical, data)    
  
}

# > dim(s_t_radical)
# [1] 8848    8

goals_sub <- c('CS')
hab_components <- unique(stat_trend_layers$component_id[stat_trend_layers$goal %in% goals_sub]) 


for(comp in hab_components) { # comp="hab_extent"
  data <- read.csv(sprintf('eez%s/layers/%s.csv', scenario, comp)) %>%
    filter(habitat %in% c('mangrove','saltmarsh','seagrass'))
  
  names(data)[which(names(data)=="rgn_id")] <- "region_id"
  names(data)[which(!(names(data) %in% c("region_id", "habitat")))] <- "value"
  data$component_id = comp
  data$component_name = paste0(layers$component_name[layers$component_id == comp], ": ", data$habitat)  
  data$goal = 'CS'
  data$subcomponent_id = data$habitat
  data$component_label = paste0(unique(layers$component_label[layers$component_id == comp & layers$goal=="CS"]), 
                                ": ", data$habitat)
  data$url = unique(layers$url[layers$component_id == comp])
  
  data <- data %>%
    select(region_id, value, component_id, component_name, component_label, goal, url, subcomponent_id) %>%
    unique()
  
  s_t_radical <- rbind(s_t_radical, data)    
  
}

# > dim(s_t_radical)
# [1] 9841    8

## getting the rest of the data
stat_trend_layers <- stat_trend_layers %>%
  select(component_id, goal, dimension, scenario, units, source, url)
  

s_t_radical <- stat_trend_layers %>%
  left_join(s_t_radical) 
# > dim(s_t_radical)
# [1] 9841   12

#-----------------------------------------------------------------------------------
### formatting
#-----------------------------------------------------------------------------------

## combine status/trend and pressures/resilience data

radical <- p_r_radical %>%
  bind_rows(s_t_radical)   %>%
  filter(!is.na(region_id)) %>%
  filter(region_id <= 250) %>%
  filter(region_id != 213) %>%
  select(component_id, subcomponent_id, component_name, component_label, goal, dimension, scenario, region_id, value, units, source, url)
 
#-----------------------------------------------------------------------------------
### adding trend data
#-----------------------------------------------------------------------------------

trends <-  read.csv(sprintf('eez%s/scores.csv', scenario)) %>%
  filter(dimension=="trend") %>%
  mutate(component_id = paste0("trend_", goal)) %>%
  mutate(subcomponent_id = NA) %>%
  mutate(component_name = paste0("trend: ", goal)) %>%
  mutate(component_label = paste0("Calculated Trend: ", goal)) %>%
  mutate(scenario = "2015") %>%
  mutate(units = NA) %>%
  mutate(source = "toolbox calculation: change in status during past 5 years") %>%
  mutate(url = NA) %>%
  select(component_id, subcomponent_id, component_name, component_label, goal, dimension, scenario, region_id, value=score, units, source, url)
  
radical_final <- rbind(radical, trends)

#-----------------------------------------------------------------------------------
### adding score/status data for subgoals/goals without data layers that can easily be used
#-----------------------------------------------------------------------------------

scores <-  read.csv(sprintf('eez%s/scores.csv', scenario)) %>%
  filter(goal %in% c('FIS', 'LIV', 'ECO', 'MAR', 'ICO', 'SPP')) %>%
  filter(dimension=="score") %>%
  mutate(component_id = paste0("score_", goal)) %>%
  mutate(subcomponent_id = NA) %>%
  mutate(component_name = paste0("score: ", goal)) %>%
  mutate(component_label = paste0("Calculated Score: ", goal)) %>%
  mutate(scenario = "2015") %>%
  mutate(units = NA) %>%
  mutate(source = "toolbox calculated value") %>%
  mutate(url = NA) %>%
  mutate(score = score/100) %>%  ### NOTE: This is in response to the website needing all values between 0-1
  select(component_id, subcomponent_id, component_name, component_label, goal, dimension, scenario, region_id, value=score, units, source, url)

radical_final <- rbind(radical_final, scores)


status <-  read.csv(sprintf('eez%s/scores.csv', scenario)) %>%
  filter(goal %in% c('FIS', 'LIV', 'ECO', 'MAR', 'ICO', 'SPP')) %>%
  filter(dimension=="status") %>%
  mutate(component_id = paste0("status_", goal)) %>%
  mutate(subcomponent_id = NA) %>%
  mutate(component_name = paste0("status: ", goal)) %>%
  mutate(component_label = paste0("Calculated Status: ", goal)) %>%
  mutate(scenario = "2015") %>%
  mutate(units = NA) %>%
  mutate(source = "toolbox calculated value") %>%
  mutate(url = NA) %>%
  mutate(score = score/100) %>%  ### NOTE: This is in response to the website needing all values between 0-1
  select(component_id, subcomponent_id, component_name, component_label, goal, dimension, scenario, region_id, value=score, units, source, url)

radical_final <- rbind(radical_final, status)


#-----------------------------------------------------------------------------------
### final formatting
#-----------------------------------------------------------------------------------
radical_final <-  radical_final %>%
  mutate(radical_component_id = paste(component_id, subcomponent_id, goal, substring(dimension, 1,1), sep="_")) %>%
  select(ohi_component_id = component_id, component_id = radical_component_id, subcomponent_id, 
         component_label, component_name, goal, dimension, scenario, region_id, value, units, source, url)

# > dim(radical_final)
# [1] 60142    13
  
## data check ##
data.frame(filter(radical_final, goal=="CW" & region_id==1) %>%
  select(-source))
data.frame(filter(radical_final, goal=="CP" & region_id==7) %>%
  select(-source) %>%
  arrange(dimension))
data.frame(filter(radical_final, goal=="CS" & region_id==163) %>%
             select(-source) %>%
             arrange(dimension))
data.frame(filter(radical_final, goal=="HAB" & region_id==163) %>%
             select(-source) %>%
             arrange(dimension))
data.frame(filter(radical_final, goal=="FIS" & region_id==163) %>%
             select(-source, -component_name) %>%
             arrange(dimension))

# all the combinations of data seem to match
setdiff(layers$component_id, radical_final$ohi_component_id)
setdiff(radical_final$ohi_component_id, layers$component_id)  # trend data is ok and scores for ECO, FIS, ICO, LIV, MAR, SPP...this was added secondarily

sum(duplicated(paste(radical_final$component_id, radical_final$region_id)))
## end data check

write.csv(radical_final, sprintf('%s/radicalv2_%s_%s.csv', saveFile, scenario, Sys.Date()),
          row.names=FALSE, na="") 
}