## Compare fisheries scores between this year and last
## commit for comparison: e0ed46b

compare <- read.csv("score_check/fis_compare_diff_data_2018-11-19.csv") 

library(ggplot2)
p <- ggplot(dplyr::filter(compare, year==2017 & dimension=="status" & goal == "FIS"), aes(x=old_score, y=score)) +
  geom_point(aes(text = paste0("rgn = ", rgn_name)), shape=19) +
  geom_abline(slope=1, intercept=0) +
  theme_bw()

plotly_fig <- plotly::ggplotly(p)
htmlwidgets::saveWidget(plotly::as_widget(plotly_fig), "tmp_file.html", 
                        selfcontained = TRUE)

### Compare scores with no underharvest penalty
compare <- read.csv("score_check/fis_no_underharvest_diff_data_2018-11-19.csv") 

library(ggplot2)
p <- ggplot(dplyr::filter(compare, year==2018 & dimension=="status" & goal == "FIS"), aes(x=old_score, y=score)) +
  geom_point(aes(text = paste0("rgn = ", rgn_name)), shape=19) +
  geom_abline(slope=1, intercept=0) +
  theme_bw()

plotly_fig <- plotly::ggplotly(p)
htmlwidgets::saveWidget(plotly::as_widget(plotly_fig), "tmp_file.html", 
                        selfcontained = TRUE)

### Compare score with no catch weighting (avg. of B/Bmsy values)

