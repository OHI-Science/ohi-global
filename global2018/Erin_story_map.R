# preparing some data for Erin: story map

library(dplyr)
library(tidyr)
library(sf)
library(sp)

source("https://raw.githubusercontent.com/OHI-Science/ohiprep_v2018/gh-pages/src/R/spatial_common.R")


# ohi scores
scores <- read.csv(here("global2018/scores_2018-11-28.csv")) %>%
  filter(region_id != 0) %>%
  filter(year == 2018) %>%
  filter(dimension == "score") %>%
  spread(goal, score) %>%
  select(-dimension, -year) %>%
  rename(rgn_id = region_id)

# trend estimates
trend_df <- read.csv(here("global2018/Results/data/trends_2018.csv")) %>%
  select(rgn_id=region_id, trend_score = Index)

scores <- scores %>%
  left_join(trend_df, by="rgn_id")


# join with region data
regions_with_data <- regions %>%
  left_join(scores, by="rgn_id")


# convert to latlong
regions_lat_long <- st_transform(regions_with_data, "+init=epsg:4326")

## save as shapefile
st_write(regions_lat_long, dsn=file.path("/home/shares/web/data/htdocs/data"),
         layer = "ohi_2018_data_lat_long",
         driver="ESRI Shapefile")

setwd("/home/shares/web/data/htdocs/data") # NOTE: have to set working directory because you can't have paths in zipfile
zip(zipfile = "ohi_2018_story_map_lat_long",
    files = c("ohi_2018_data_lat_long.dbf",
              "ohi_2018_data_lat_long.prj",
              "ohi_2018_data_lat_long.shp",
              "ohi_2018_data_lat_long.shx"))

file.remove(c("ohi_2018_data_lat_long.dbf",
          "ohi_2018_data_lat_long.prj",
          "ohi_2018_data_lat_long.shp",
          "ohi_2018_data_lat_long.shx"))

#####################
### light version

regions_light <- readOGR(dsn = "../ohiprep_v2018/globalprep/spatial/v2015/downres", layer = "rgn_all_gcs_low_res")
proj4string(regions_light) <- CRS("+init=epsg:4326")

regions_light@data$rgn_id <- as.numeric(as.character(regions_light@data$rgn_id))
summary(regions_light@data$rgn_id)

# join with region data
regions_light@data <- regions_light@data %>%
  left_join(scores, by="rgn_id")


## save as shapefile
writeOGR(regions_light, dsn=file.path("/home/shares/web/data/htdocs/data"),
         layer = "ohi_2018_data_lat_long_light",
         driver="ESRI Shapefile")

setwd("/home/shares/web/data/htdocs/data") # NOTE: have to set working directory because you can't have paths in zipfile
zip(zipfile = "ohi_2018_story_map_lat_long_light",
    files = c("ohi_2018_data_lat_long_light.dbf",
              "ohi_2018_data_lat_long_light.prj",
              "ohi_2018_data_lat_long_light.shp",
              "ohi_2018_data_lat_long_light.shx"))

file.remove(c("ohi_2018_data_lat_long_light.dbf",
              "ohi_2018_data_lat_long_light.prj",
              "ohi_2018_data_lat_long_light.shp",
              "ohi_2018_data_lat_long_light.shx"))
