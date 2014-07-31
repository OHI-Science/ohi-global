library(plyr)
library(dplyr)

step = '3-q90'

setwd('~/github/ohi-global/eez2013')

d = read.csv('scores.csv') %>%
  filter(goal=='TR' & dimension=='status')

# csv
csv = sprintf('temp/scores_TR_%s.csv', step)
write.csv(d, csv, row.names=F, na='')

# png hist
png(sprintf('temp/scores_TR_%s_hist.png', step))
h = hist(d$score, breaks=c(seq(0, 90, by=10), 99, 100))
plot(h, freq=T, col='gray', main=sprintf('Histogram of TR_%s', step), xlab='Score')
text(h$mids, h$counts+2, label=c(h$counts))
dev.off()

# TODO: newer method
# dd<-data.frame(x=rnorm(100,5,2))
# ggplot(dd, aes(x=x))+ geom_histogram() +
#   stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5) 

system(sprintf('open %s', csv))
system(sprintf('open %s', png))