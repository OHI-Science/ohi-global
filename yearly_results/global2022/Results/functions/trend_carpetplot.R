# creates a carpet plot of trends

data <- read.csv(here(sprintf('%s/OHI_final_formatted_scores_%s.csv', saveFile, dateFile))) 

data <- data %>%
  filter(dimension == "score") %>%   # focus only on score data
  filter(region_id != 0) %>%         # this weighted mean includes high seas and Antarctica
  mutate(region_id = ifelse(region_id==300, 0, region_id)) %>%   #convert the 300 (i.e., only eez's averaged to zero)
  filter(region_id <= 250) %>%       # get rid of high seas regions
  filter(region_id != 213)  %>% # get rid of antarctica
  mutate(value = round(value, 0)) %>%
  group_by(region_id) %>% 
  mutate(meanIndex=mean(value[goal=="Index"])) %>%
  ungroup() %>%
  data.frame()


trends <- read.csv(sprintf("%s/Results/data/trends_%s.csv", saveFile, scenario)) %>%
  gather(goal, "value", -(1:2)) %>%
  select(-region_id, region_name = country) %>%
  filter(region_name != "eez_weighted_avg")

## order regions by trend from decreasing to increasing (use in factor levels, below)
region_order <- trends %>%
  filter(goal=="Index") %>%
  arrange(value)

values <-   brewer.pal(11, "RdYlBu")
col.values <- colorRampPalette(values, space = 'Lab')(12)
col.brks  <- c(-100, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 100)

trends$goal <- factor(trends$goal, levels = goals)
trends$region_name <- factor(trends$region_name, levels=region_order$region_name)
trends$trend_category <- cut(trends$value, breaks = col.brks)


p <- ggplot(trends, aes(y=region_name, x=goal, fill=trend_category)) + 
  geom_tile(aes(text = paste0("trend = ", value))) +
  scale_fill_manual(values = col.values, na.value="black") +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
  ylab("") + 
  xlab("") +
  theme_bw()
plot(p)

ggsave(sprintf("%s/Results/figures/trend_heatplot.png", saveFile), 
       width = 20, height=25, units="in")
#ggplotly(p)
