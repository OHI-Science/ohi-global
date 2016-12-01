### Croscon test
### What will happen if high seas and antarctica data are excluded?
library(dplyr)

data <- read.csv("global2016/radical_2016-11-17.csv")
head(data)

## eliminate:
data <- data %>%
  filter(region_id != 0) %>%
  filter(region_id <= 250 | region_id==300) %>%
  filter(region_id != 213)

write.csv(data, "radical_2016-11-17_no_HS-AQ_test.csv", row.names=FALSE, na="")

## convert to NA
data <- read.csv("global2016/radical_2016-11-17.csv")
head(data)

data2 <- data %>%
  # mutate(value = ifelse(region_id == 0, NA, value)) %>%
  mutate(value = ifelse(region_id > 250 & region_id < 300, NA, value)) %>%
  mutate(value = ifelse(region_id == 213, NA, value))

compare <- data %>%
  select(scenario, goal, dimension, region_id, old_value = value) %>%
  left_join(data2, by=c("scenario", "goal", "dimension", "region_id")) %>%
  mutate(diff = ifelse(old_value != value, "different", "same"))
   
tmp1 <- filter(compare, is.na(value) & !is.na(old_value) )             
tmp2 <- filter(compare, !is.na(value) & is.na(old_value) )
tmp3 <- filter(compare, value != old_value)
tmp2 <- compare[compare$old_value != compare$value, ]
       
write.csv(data2, "global2016/radical_2016-11-17_blanks_for_HS-AQ_test.csv", row.names=FALSE, na = "")

