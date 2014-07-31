# merge_scores.r
# this script creates a global 2014 scores.csv, which is a combination of eez2014, antarctica2014, highseas2014

# don't need to set working directory; just need to save it in global2014

# merge scores (cut region_ids of zero.  We can add these, but the region_id's will need to be changed.)
d = rbind_list(
  read.csv('eez2014/scores.csv', stringsAsFactors=FALSE) %>%
    filter(!(region_id %in% c(0, 213))), #region 213 is replaced with specific Antarctica data below.
  read.csv('highseas2014/scores.csv', stringsAsFactors=FALSE) %>%
    filter(region_id != 0),
  read.csv('antarctica2014/scores.csv', stringsAsFactors=FALSE) %>%
    filter(region_id==0) %>%
    mutate(
      region_id = 213))  #note: has fewer variables than other regions (no pressures, resilience, trend because these don't average well)

# write scores
# save this in global2014
write.csv(d, file.path('global2014', 'scores.csv'), row.names=F, na='')

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