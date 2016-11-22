###############################################
## Preparing data for the Diversity Dashboard
###############################################
library(dplyr)

dataDate <- "2016-11-17"

data <- read.csv(sprintf("../radical_%s.csv", dataDate)) %>%
  filter(region_id != 0) %>%      # no global summary (eez, hs, ant)
  filter(region_id <= 250) %>%    # no global summary (eez only, region 300)
  filter(region_id != 213) %>%    # no Antarctica
  filter(goal == "Index") %>%     # only scores
  filter(dimension == "score")
  
summary(data)

data <- data %>%
  select(year = scenario, region_id, score=value)


#### Add in ISO3 code
iso <- read.csv('../../eez2016/layers/cntry_rgn.csv')

dups <- iso$rgn_id[duplicated(iso$rgn_id)]
filter(iso, rgn_id %in% dups)

iso <- iso %>%
  filter(!(cntry_key %in% c("Galapagos Islands", "Alaska", "Hawaii", "Trindade", "Easter Island"))) %>%
  group_by(rgn_id) %>%
  summarize(iso3 = paste(cntry_key, collapse = " ")) %>%
  select(region_id = rgn_id, iso3) %>%
  data.frame()

# check one more time for duplicates (should be none)
iso$rgn_id[duplicated(iso$rgn_id)]

data <- data %>%
  left_join(iso, by="region_id")

#### Add in country name
rgn_name <- read.csv('../../eez2016/layers/rgn_global.csv') %>%
  select(region_id = rgn_id, region_name = label)

data <- data %>%
  left_join(rgn_name, by = "region_id")

### Organize data:
data <- data %>%
  select(year, region_id, iso3, region_name, score)
  
write.csv(data, sprintf("../DiversityDashboard/OHI_data_%s.csv", dataDate), row.names=FALSE)

##################################################
##### Preparing spatial data
###################################################
source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(sp)

# I think for this, we just want the EEZs.  I'll also include the land.  

ohi_regions <- readOGR(dsn = file.path(dir_M, "git-annex/globalprep/spatial/d2014/data"), layer="regions_gcs")  
table(ohi_regions@data$rgn_typ)

ohi_regions_DD <- ohi_regions[ohi_regions@data$rgn_typ %in% c("eez", "land", "land-disputed", "land-noeez"), ]
table(ohi_regions_DD@data$ant_typ)

ohi_regions_DD <- ohi_regions_DD[ohi_regions_DD@data$ant_typ != "eez-ccamlr", ]
table(ohi_regions_DD@data$rgn_typ)
table(ohi_regions_DD@data$ant_typ)

head(ohi_regions_DD)

data_map <- data %>%
  filter(year==2016) %>%
  dplyr::select(region_id, iso3, region_name)

ohi_regions_DD@data <- ohi_regions_DD@data %>%
  dplyr::select(rgn_typ, region_id = rgn_id) %>%
  left_join(data_map, by="region_id")

writeOGR(ohi_regions_DD, 
         dsn = file.path(dir_M, "git-annex/globalprep/data_requests/diversity_dashboard"), 
                         layer = "OHI_region_map", driver = 'ESRI Shapefile', overwrite_layer = TRUE)