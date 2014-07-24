setwd('~/github/ohi-global/global2014')

# merge scores
d = rbind_list(
  read.csv('../eez2014/scores.csv'),
  read.csv('../highseas2014/scores.csv'),
  read.csv('../antarctica2014/scores.csv') %>%
    filter(region_id==0) %>%
    mutate(
      region_id = 213))

# write scores
write.csv(d, 'scores.csv', row.names=F, na='')

# output file for Radical...

# a = read.csv('../antarctica2014/scores.csv')
# > filter(a, region_id %in% c(0,248100) & goal=='BD') %>% arrange(region_id, goal, dimension)
#   goal  dimension region_id score
# 1   BD     future         0 95.87
# 2   BD      score         0 93.63
# 3   BD     status         0 91.39
# 4   BD     future    248100 96.24
# 5   BD  pressures    248100    NA
# 6   BD resilience    248100    NA
# 7   BD      score    248100 92.83
# 8   BD     status    248100 89.43
# 9   BD      trend    248100  0.01