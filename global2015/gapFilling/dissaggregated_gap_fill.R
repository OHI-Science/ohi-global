#################################################################
## Description of regions that were gap-filled
## when they were "disaggreted" from the 2012 to 2013 analysis
###############################################################

regions <- read.csv('src/LookupTables/eez_rgn_2013master.csv') %>%
  filter(rgn_typ == "eez") %>%
  filter(rgn_id_2013 != 255)

dups2012 <- regions$region_id_2012[duplicated(regions$region_id_2012) ]

regions <- regions %>%
  filter(region_id_2012 %in% dups2012) %>%
  arrange(sov_id, as.numeric(region_id_2012)) %>%
  select(rgn_id_2013, rgn_nam_2013, sov_id, sov_nam, region_id_2012)

regions <- regions %>%
  filter(rgn_id_2013 != sov_id) %>%
  unique()

write.csv(regions, '../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv', row.names=FALSE)
