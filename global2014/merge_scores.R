 # merge_scores.r
# this script creates a global 2014 scores.csv, which is a combination of eez2014, antarctica2014, highseas2014
require(foreign)

area <- read.dbf(file.path(dir_neptune_data, "git-annex/Global/NCEAS-Regions_v2014/data/rgn_gcs.dbf"))
area <- area %>% 
  filter(rgn_type %in% c('eez', 'fao')) %>%
  select(region_id=rgn_id, area_km2)


# merge scores (cut region_ids of zero.  We can add these, but the region_id's will need to be changed.)
d = rbind_list(
  read.csv('eez2014/scores.csv', stringsAsFactors=FALSE) %>%
    filter(!(region_id %in% c(0, 213))) %>%
    mutate(rgn_type = "eez"), #region 213 is replaced with specific Antarctica data below.
 read.csv('highseas2014/scores.csv', stringsAsFactors=FALSE) %>%
   filter(region_id != 0) %>%
   mutate(rgn_type = "fao"),
 read.csv('antarctica2014/scores.csv', stringsAsFactors=FALSE) %>%
   filter(region_id==0) %>%
   mutate(region_id = 213) %>%
   mutate(rgn_type = "eez")
    )  #note: has fewer variables than other regions (no pressures, resilience, trend because these don't average well)

# # checking against old data...remove the antarctica and high seas for testing


d = d %>%
  left_join(area, by="region_id")

# global goal scores
GlobalGoalScores <- d %>%
  group_by(goal, dimension) %>%
  filter(goal != "Index") %>%
  summarize(score = round(weighted.mean(score, area_km2, na.rm=TRUE), 2)) %>%
  filter(dimension %in% c('status', 'future', 'score')) %>%
  ungroup() %>%
  mutate(rgn_type="global", region_id=0, area_km2=NA) %>%
  select(goal, dimension, region_id, score, rgn_type, area_km2)

RegionalGoalScores <- d %>%
  group_by(rgn_type, goal, dimension) %>%
  filter(goal != "Index") %>%
  summarize(score = round(weighted.mean(score, area_km2, na.rm=TRUE), 2)) %>%
  filter(dimension %in% c('status', 'future', 'score')) %>%
  ungroup() %>%
  mutate(region_id=0, area_km2=NA) %>%
  select(goal, dimension, region_id, score, rgn_type, area_km2)

# tmp <- read.csv('eez2014/scores.csv', stringsAsFactors=FALSE)
# # test <- GlobalGoalScores %>%
# #   left_join(tmp)
# # max(abs(test$score-test$score2))
# tmp[tmp$goal=="Index" & tmp$region_id==0,]
# tmp[tmp$goal=="Index",]

# global Index scores
GlobalIndexScores <- d %>%
  filter(dimension %in% c("future", "score"),
         goal %in% 'Index') %>%
  group_by(dimension) %>%
  summarize(score = round(weighted.mean(score, area_km2, na.rm=TRUE), 2)) %>%  
  ungroup() %>%
  mutate(goal="Index", rgn_type="global", region_id=0, area_km2=NA) %>%
  select(goal, dimension, region_id, score, rgn_type, area_km2)


RegionalIndexScores <- d %>%
  filter(dimension %in% c("future", "score"),
         goal %in% 'Index') %>%
  group_by(rgn_type, dimension) %>%
  summarize(score = round(weighted.mean(score, area_km2, na.rm=TRUE), 2)) %>%  
  ungroup() %>%
  mutate(goal="Index", region_id=0, area_km2=NA) %>%
  select(goal, dimension, region_id, score, rgn_type, area_km2)

scores <- rbind(d, GlobalGoalScores, RegionalGoalScores, RegionalIndexScores, GlobalIndexScores)
scores <- scores %>%
  mutate(scenario="2014") %>%
  select(scenario, region_type=rgn_type, goal, dimension, region_id, score) %>%
  arrange(scenario, region_type, region_id)

# write scores
# save this in global2014
write.csv(scores, file.path('global2014', sprintf('scores_2014_%s.csv', format(Sys.Date(), '%Y-%m-%d'))), row.names=F, na='')



#### for Radical ----
dir_og = '../ohi-global'

s2014 <- read.csv('global2014/scores_2014_2014-09-05.csv')

## remove eez/fao summaries (include only the global summaries)
s2014 <- s2014 %>%
  filter(!(region_id==0 & region_type %in% c('eez', 'fao')))

s2012 <- read.csv(file.path(dir_og, 'eez2012/scores.csv')) %>% 
  mutate(scenario=2012, region_type="eez") 

radical <- read.csv(file.path(dir_og, 'eez2013/scores.csv')) %>%
  mutate(scenario = 2013, region_type="eez") %>%
  rbind(s2012) %>%
  rbind(s2014) %>%
  select(scenario, goal, dimension, region_type, region_id, value=score) %>%
  mutate(dimension=revalue(dimension, c('future'="likely_future_state"))) %>%
  mutate(value=round(value, 2)) %>%
  arrange(scenario, goal, dimension, region_id)

rad_region_0 <- radical[radical$region_id==0,]
rad_region_Index <- radical[(radical$goal=="Index" & radical$region_id!=0),]

radical_simple <- radical %>%
  filter(region_id != 0) %>%
  filter(goal != "Index")
table(radical_simple$goal)
table(radical_simple$region_id)

radical_grid_eez <- expand.grid(scenario=c(2012,2013,2014), 
                            goal=unique(radical_simple$goal), 
                            dimension=unique(radical_simple$dimension),
                            region_id=unique(radical_simple$region_id[radical_simple$region_type=="eez"]),
                            region_type="eez")

radical_grid_fao <- expand.grid(scenario=c(2012,2013,2014), 
                                goal=unique(radical_simple$goal), 
                                dimension=unique(radical_simple$dimension),
                                region_id=unique(radical_simple$region_id[radical_simple$region_type=="fao"]),
                                region_type="fao")

radical_grid <- rbind(radical_grid_eez, radical_grid_fao)

radical_full <- merge(radical_grid, radical_simple, by=c('scenario', "goal", "dimension", "region_type", 'region_id'), all.x=TRUE)
radical_full <- rbind(radical_full, rad_region_0, rad_region_Index)

table(radical_full$goal[radical_full$region_type=="eez"])
table(radical_full$goal[radical_full$region_type=="fao"])

write.csv(radical_full, file.path("global2014", sprintf("/OHI_results_for_Radical_%s_full.csv", format(Sys.Date(), '%Y-%m-%d'))), row.names=F, na='')


# ## save to git-annex?
# csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/OHI_results_for_Radical_%s.csv',
#               dir_neptune_data, Sys.Date())
# if (file.exists(csv)) unlink(csv, force=T)
# write.csv(radical, csv, row.names=F, na='')
# 
# radical <- read.csv(file.path(dir_og, 'eez2013/scores.csv')) %>%
#   mutate(scenario = 2013) %>%
#   rbind(s2012) %>%
#   rbind(s2014) %>%
#   select(scenario, goal, dimension, region_id, value=score) %>%
#   mutate(dimension=revalue(dimension, c('future'="likely_future_state"))) %>%
#   mutate(value=round(value, 2)) %>%
#   arrange(scenario, goal, dimension, region_id)




# output file for Radical...
# ** JSL will find where a template file is that knows the format Radical wants

# a = read.csv('../antarctica2014/scores.csv')
# filter(a, region_id %in% c(0,248100) & goal=='BD') %>% arrange(region_id, goal, dimension)
# #   goal  dimension region_id score
# # 1   BD     future         0 95.87
# # 2   BD      score         0 93.63
# # 3   BD     status         0 91.39
# # 4   BD     future    248100 96.24
# # 5   BD  pressures    248100    NA
# # 6   BD resilience    248100    NA
# # 7   BD      score    248100 92.83
# # 8   BD     status    248100 89.43
# # 9   BD      trend    248100  0.01