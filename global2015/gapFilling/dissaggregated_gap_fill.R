#################################################################
## Description of regions that were gap-filled
## when they were "disaggreted" from the 2012 to 2013 analysis
###############################################################

library(dplyr)

regions <- read.csv('../ohiprep/src/LookupTables/eez_rgn_2013master.csv') %>%
  filter(rgn_typ == "eez") %>%
  filter(rgn_id_2013 != 255)

## First get the territorial regions that were disaggregated from 2012-2013
dups2012 <- regions$region_id_2012[duplicated(regions$region_id_2012) ]

regions_dis <- regions %>%
  filter(region_id_2012 %in% dups2012) %>%
  arrange(sov_id, as.numeric(region_id_2012)) %>%
  select(rgn_id_2013, rgn_nam_2013, sov_id, sov_nam, region_id_2012)

regions_dis <- regions_dis %>%
  filter(rgn_id_2013 != sov_id) %>%
  unique() %>%
  mutate(description = "Territories, disaggregated in 2013, probably gapfilled") 


## Next get Brunei, is surrounded by Malaysia and was grouped with that country in the 2012 analysis
brunei <- regions %>%   
  filter(rgn_nam_2013 == "Brunei") %>%
  select(rgn_id_2013, rgn_nam_2013, sov_id, sov_nam, region_id_2012) %>%
  mutate(description = "Country, not included in 2012, probably gapfilled")

## Next get territories that were included in the 2012 analysis (no disaggregation, but data probably not reported separately)
regions_ter <- regions %>%
  filter(rgn_id_2013 != sov_id) %>%
  filter(!(rgn_id_2013 %in% regions_dis$rgn_id_2013)) %>%
  unique() %>%
  select(rgn_id_2013, rgn_nam_2013, sov_id, sov_nam, region_id_2012) %>%
  mutate(description = "Territories, included prior to 2013, may be gapfilled with parent country") 


## Combine
regions_gf <- regions_dis %>%
  rbind(brunei) %>%
  rbind(regions_ter)


write.csv(regions_gf, '../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv', row.names=FALSE)
