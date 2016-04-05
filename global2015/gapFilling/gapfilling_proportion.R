##############################################
# figuring out the proportion of each layer
# with gapfilling
##############################################
library(dplyr)
library(tidyr)

setwd("global2015/gapFilling")
# In the future (when I do a better job of standardizing the data), 
# I will want to automate this.  But this time I am going to muscle through it

### Fisheries ----
bmsy <- read.csv('../../eez2015/layers/fis_b_bmsy.csv')

catch <- read.csv('../../eez2015/layers/fis_meancatch.csv') %>%
  separate(fao_saup_id, c("fao_id", "saup_id"), sep="_") %>%
  separate(taxon_name_key, c("taxon_name", "taxon_key"), sep="_") %>%
  mutate(fao_id = as.numeric(fao_id)) %>%
  mutate(saup_id = as.numeric(saup_id)) %>%
  mutate(taxon_key = as.numeric(taxon_key)) %>%
  left_join(bmsy, by=c("fao_id", "taxon_name", "year")) %>%
  mutate(gapfill = ifelse(is.na(b_bmsy), 1, 0)) %>%
  filter(year == 2011)

mean(catch$gapfill)

data <- data.frame(layer = c("Fisheries catch data", "Stock exploitation status (B/Bmsy)"), 
                   proportion_gf = c(0, mean(catch$gapfill)),
                   component = "goal")

### Mariculture ----
sustain <- read.csv('layers/mar_sustainability_score.csv') %>%
  select(region_id = id, species, species_code, year, gap_fill_sustain = gapfill) %>%
  mutate(gap_fill_sustain = ifelse(gap_fill_sustain == "actuals", 0, 1)) %>%
  filter(year==2013)

data <- rbind(data, data.frame(layer = c("Mariculture Sustainability Index (MSI)"),
                               proportion_gf = c(mean(sustain$gap_fill_sustain)),
                               component = "goal"))

harvest <- read.csv('layers/mar_harvest_tonnes.csv') %>%
  select(region_id = id, species, species_code, year, gap_fill_harvest = gap_0_fill)%>%
  mutate(gap_fill_harvest = ifelse(gap_fill_harvest == "NA_to_zero", 1, 0)) %>%
  filter(year==2013)

data <- rbind(data, data.frame(layer = "Mariculture yield (tonnes)",
                               proportion_gf = mean(harvest$gap_fill_harvest),
                               component = "goal"))

### Artisanal Opportunities ----
ao_access <- read.csv('layers/ao_access.csv')

data <- rbind(data, data.frame(layer = "AO access",
                               proportion_gf = mean(ao_access$value),
                               component = "goal"))

ao_need <- read.csv('layers/ao_need.csv')

data <- rbind(data, data.frame(layer = "AO need",
                               proportion_gf = mean(ao_need$gapfill),
                               component = "goal"))

### Natural products ----

# harvest data
harvest <- read.csv('layers/np_harvest_tonnes_relative.csv') %>%
  filter(gapfill != "r2_u_gr") %>%   
  mutate(gapfill = ifelse(gapfill == "none", 0, 1)) %>%
  filter(year==max(year))

data <- rbind(data, data.frame(layer = "NP harvest",
                               proportion_gf = mean(harvest$gapfill),
                               component = "goal")) 

# exposure data
exposure <- read.csv('../../eez2015/temp/NP_exposure_gapfill.csv') %>%
  mutate(exposure = ifelse(gap_fill==0, 0, 1)) %>%
  select(rgn_id, product, year, exposure)

# fish scores
fish <- read.csv('scores.csv') %>%
  filter(dimension == "status", goal == "FIS", region_id != 0) %>%
  mutate(product = "fish oil") %>%
  mutate(exposure = score/100) %>%
  mutate(year = max(exposure$year)) %>%
select(rgn_id=region_id, product, year, exposure)

exposure <- exposure %>%
  rbind(fish) %>%
  filter(year==(max(year)))

data <- rbind(data, data.frame(layer = "NP exposure",
                               proportion_gf = mean(exposure$exposure, na.rm=TRUE),
                               component = "goal")) 

# risk data
# No gapfilling for risk (assigned 1 or 0 values and used blast/cyanide for ornamentals)

data <- rbind(data, data.frame(layer = "NP risk",
                               proportion_gf = 0,
                               component = "goal")) 

### Habitats
# extent was not gapfilled (except in the rocky reef case)
data <- rbind(data, data.frame(layer = c("rocky_reef: extent",
                                         "coral: extent",
                                         "mangrove: extent",
                                         "saltmarsh: extent",
                                         "seagrass: extent",
                                         "seaice_edge: extent",
                                         "seaice_shoreline: extent",
                                         "soft_bottom: extent"),
                               proportion_gf = 0,
                               component = "goal")) 


## condition
condition_gf <- read.csv('layers/hab_health.csv') %>%
  select(rgn_id, habitat, gap_fill_health=gap_fill) %>%
  filter(!is.na(gap_fill_health)) %>%
  mutate(gap_fill_health = ifelse(gap_fill_health==0, 0, 1)) %>%
    group_by(habitat) %>%
 summarize(proportion_gf = mean(gap_fill_health)) %>%
  mutate(layer = paste0(habitat, ": condition")) %>%
  select(layer, proportion_gf) %>%
  mutate(component = "goal") %>%
  data.frame()

data <- rbind(data, condition_gf)

## condition
condition_gf <- read.csv('layers/hab_health.csv') %>%
  select(rgn_id, habitat, gap_fill_health=gap_fill) %>%
  filter(!is.na(gap_fill_health)) %>%
  mutate(gap_fill_health = ifelse(gap_fill_health==0, 0, 1)) %>%
  group_by(habitat) %>%
  summarize(proportion_gf = mean(gap_fill_health)) %>%
  mutate(layer = paste0(habitat, ": condition")) %>%
  select(layer, proportion_gf) %>%
  mutate(component = "goal") %>%
  data.frame()

data <- rbind(data, condition_gf)

## trend
trend_gf <-  read.csv('layers/hab_trend.csv') %>%
  select(rgn_id, habitat, gap_fill_trend = gap_fill)%>%
  filter(!is.na(gap_fill_trend)) %>%
  mutate(gap_fill_trend = ifelse(gap_fill_trend==0, 0, 1)) %>%
  group_by(habitat) %>%
  summarize(proportion_gf = mean(gap_fill_trend)) %>%
  mutate(layer = paste0(habitat, ": trend")) %>%
  select(layer, proportion_gf) %>%
  mutate(component = "goal") %>%
  data.frame()

data <- rbind(data, trend_gf)

### TR ----
tr_sus <-  read.csv('layers/tr_sustainability.csv')
tr_sus <- separate(tr_sus, gaps, c("Ep", "U", "S", "L", "G"), sep=c(1,2,3,4))

tr_sus <- tr_sus %>%  ## L and U don't actually matter
  mutate(Ep = ifelse(Ep == "E", NA, 
                     ifelse(Ep == "_", 0, 1))) %>%  ## E values are given an NA value
  mutate(S = ifelse(S == "S", NA,                   ## S values are given an NA value
                    ifelse(S == "_", 0, 1))) %>%
  mutate(G = ifelse(G %in% c("_", "*", "c"), 0,
                    ifelse(G == "G", NA, 1)))  %>%    ## G values are given an NA value, c is an alternative data source, * means it wasn't needed
  mutate(SG = paste0(S, G)) %>%  # Informational: for now, estimates of S are treated the same, regardless of whether the gdp is estimated                                                      
  filter(year == max(year))

data <- rbind(data, data.frame(layer = c("tr_sustainability",
                                         "tr_employment"),
                               proportion_gf = c(mean(tr_sus$S, na.rm=TRUE),
                                                 mean(tr_sus$Ep, na.rm=TRUE)),
                               component = "goal")) 


### Species ----
data <- rbind(data, data.frame(layer = c("ico_species_list",
                                         "ico_spp_ranges",
                                         "ico_spp_status",
                                         "ico_spp_trend"),
                               proportion_gf = 0,
                               component = "goal"))

### CW ----

# nutrient/fertilizer
fert <- read.csv('layers/po_nutrients_3nm.csv')

data <- rbind(data, data.frame(layer = c("nutrient pollution"),
                               proportion_gf = mean(fert$gapfill),
                               component = "goal"))

# pesticide
pest <- read.csv('../../../ohiprep/globalprep/PressuresRegionExtract/data/cw_pesticide_score_2015_gf.csv')
data <- rbind(data, data.frame(layer = c("chemical pollution: organic"),
                               proportion_gf = mean(pest$gapfill),
                               component = "goal"))

# other chemical contributers:
data <- rbind(data, data.frame(layer = c("chemical pollution: shipping", 
                                         "chemical pollution: land-based inorganic"),
                               proportion_gf = 0,
                               component = "goal"))

## pathogens
# combination of population and proportion of people without sanitation.
# reported separately in document, so multiply 0.5 values by two to get 
# only the proportion population without sanitation gapfilling values

pathogen <- read.csv('layers/po_pathogens.csv') %>%
  mutate(pressure_score = pressure_score*2)

data <- rbind(data, data.frame(layer = c("pathogens: sanitation", 
                                         "pathogens: coastal population density"),
                               proportion_gf = c(mean(pathogen$pressure_score),
                                                 0),
                               component = "goal"))

## trash
trash <- read.csv('layers/po_trash.csv')
data <- rbind(data, data.frame(layer = "trash",
                               proportion_gf = mean(trash$pressure_score),
                               component = "goal"))

## coastal population
population <- read.csv('layers/mar_coastalpopn_inland25mi.csv') %>%
  filter(year==max(year))

data <- rbind(data, data.frame(layer = "coastal population",
                               proportion_gf = mean(population$popsum),
                               component = "goal"))

### LSP MPAs ----
data <- rbind(data, data.frame(layer = "marine and coastal protected areas",
                               proportion_gf = 0,
                               component = "goal"))

#### pressures ----

aliens <- read.csv('layers/sp_alien.csv')
data <- rbind(data, data.frame(layer = "alien pressures",
                               proportion_gf = mean(aliens$pressures.score),
                               component = "pressure"))

artisanal_hb <- read.csv('layers/fp_art_hb.csv')
data <- rbind(data, data.frame(layer = "artisanal hb",
                               proportion_gf = mean(artisanal_hb$pressures.score),
                               component = "pressure"))

artisanal_lb <- read.csv('layers/fp_art_lb.csv') 
data <- rbind(data, data.frame(layer = "artisanal lb",
                               proportion_gf = mean(artisanal_lb$pressure.score),
                               component = "pressure"))

commercial_hb <- read.csv('layers/fp_com_hb.csv') 
data <- rbind(data, data.frame(layer = "commercial hb",
                               proportion_gf = mean(commercial_hb$pressure_score),
                               component = "pressure"))

commercial_lb <- read.csv('layers/fp_com_lb.csv') 
data <- rbind(data, data.frame(layer = "commercial lb",
                               proportion_gf = mean(commercial_lb$pressure_score),
                               component = "pressure"))

genetic_escapes <- read.csv('layers/sp_genetic.csv')
data <- rbind(data, data.frame(layer = "genetic_escapes",
                               proportion_gf = mean(genetic_escapes$pressures.score, na.rm=TRUE),
                               component = "pressure"))

hd_intertidal <- read.csv('layers/hd_intertidal.csv')
data <- rbind(data, data.frame(layer = "hd_intertidal",
                               proportion_gf = mean(hd_intertidal$pressure_score, na.rm=TRUE),
                               component = "pressure"))

hd_subtidal_hb <- read.csv('layers/hd_subtidal_hb.csv')
data <- rbind(data, data.frame(layer = "hd_subtidal_hb",
                               proportion_gf = mean(hd_subtidal_hb$pressures.score, na.rm=TRUE),
                               component = "pressure"))

hd_subtidal_sb <- read.csv('layers/hd_subtidal_sb.csv')
data <- rbind(data, data.frame(layer = "hd_subtidal_sb",
                               proportion_gf = mean(hd_subtidal_sb$pressures.score, na.rm=TRUE),
                               component = "pressure"))

oa <- read.csv('layers/cc_acid.csv')
data <- rbind(data, data.frame(layer = "cc_acid",
                               proportion_gf = mean(oa$pressure_score, na.rm=TRUE),
                               component = "pressure"))

slr <- read.csv('layers/cc_slr.csv')
data <- rbind(data, data.frame(layer = "cc_slr",
                               proportion_gf = mean(slr$pressure_score, na.rm=TRUE),
                               component = "pressure"))

sst <- read.csv('layers/cc_sst.csv')
data <- rbind(data, data.frame(layer = "cc_sst",
                               proportion_gf = mean(sst$pressure_score, na.rm=TRUE),
                               component = "pressure"))

uv <- read.csv('layers/cc_uv.csv')
data <- rbind(data, data.frame(layer = "cc_uv",
                               proportion_gf = mean(uv$pressure_score, na.rm=TRUE),
                               component = "pressure"))

chem <- read.csv('layers/po_chemicals.csv')
data <- rbind(data, data.frame(layer = "po_chemical",
                               proportion_gf = mean(chem$gapfill, na.rm=TRUE),
                               component = "pressure"))

nutrient <- read.csv('layers/po_nutrients.csv')
data <- rbind(data, data.frame(layer = "po_nutrients",
                               proportion_gf = mean(nutrient$gapfill, na.rm=TRUE),
                               component = "pressure"))

pathogens <- read.csv('layers/po_pathogens.csv')
data <- rbind(data, data.frame(layer = "po_pathogens",
                               proportion_gf = mean(pathogens$pressure_score, na.rm=TRUE),
                               component = "pressure"))

trash <- read.csv('layers/po_trash.csv')
data <- rbind(data, data.frame(layer = "po_trash",
                               proportion_gf = mean(trash$pressure_score, na.rm=TRUE),
                               component = "pressure"))

target <- read.csv('layers/fp_targetharvest.csv')
data <- rbind(data, data.frame(layer = "target harvest",
                               proportion_gf = mean(target$score, na.rm=TRUE),
                               component = "pressure"))


### Resilience ----
alien_res <- read.csv('layers/alien_species.csv')
data <- rbind(data, data.frame(layer = "alien_species",
                               proportion_gf = mean(alien_res$resilience.score),
                               component = "resilience"))

habitat_res <- read.csv('layers/habitat.csv')
data <- rbind(data, data.frame(layer = "habitat",
                               proportion_gf = mean(habitat_res$resilience.score),
                               component = "resilience"))

mar_res <- read.csv('layers/mariculture.csv')
data <- rbind(data, data.frame(layer = "mariculture",
                               proportion_gf = mean(mar_res$resilience.score),
                               component = "resilience"))

tourism_res <- read.csv('layers/tourism.csv')
data <- rbind(data, data.frame(layer = "tourism",
                               proportion_gf = mean(tourism_res$resilience.score),
                               component = "resilience"))

water_res <- read.csv('layers/water.csv')
data <- rbind(data, data.frame(layer = "water",
                               proportion_gf = mean(water_res$resilience.score),
                               component = "resilience"))

cites <- read.csv('layers/cites.csv')
data <- rbind(data, data.frame(layer = "cites",
                               proportion_gf = mean(cites$resilience_score),
                               component = "resilience"))

msi <- read.csv('layers/msi_gov.csv')
data <- rbind(data, data.frame(layer = "msi",
                               proportion_gf = mean(msi$resilience.score),
                               component = "resilience"))

mora <- read.csv('../../../ohiprep/globalprep/resilience_mora/v2013/data/r_mora_2013a_gf.csv')
data <- rbind(data, data.frame(layer = "mora fishery",
                               proportion_gf = mean(mora$value),
                               component = "resilience"))

wgi <- read.csv('layers/wgi_all.csv')
data <- rbind(data, data.frame(layer = "wgi",
                               proportion_gf = mean(wgi$score),
                               component = "resilience"))

data <- rbind(data, data.frame(layer = "diversity",
                               proportion_gf = 0,
                               component = "resilience"))

write.csv(data, "data_layers_gapfill.csv", row.names=FALSE)
