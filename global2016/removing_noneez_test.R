### Croscon test
### What will happen if high seas and antarctica data are excluded?
library(dplyr)

data <- read.csv("global2016/radical_2016-11-17.csv")
head(data)

## eliminate:
data <- data %>%
  filter(region_id != 0) %>%
  filter(region_id <= 250) %>%
  filter(region_id != 213)

write.csv(data, "radical_2016-11-17_no_HS-AQ_test.csv", row.names=FALSE)

## convert to NA
data <- read.csv("global2016/radical_2016-11-17.csv")
head(data)

data <- data %>%
  mutate(value = ifelse(region_id == 0, NA, value)) %>%
  mutate(value = ifelse(region_id > 250, NA, value)) %>%
  mutate(value = ifelse(region_id == 213, NA, value))
                        
write.csv(data, "radical_2016-11-17_NAs_for_HS-AQ_test.csv", row.names=FALSE)
