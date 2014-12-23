#######################################################################
### NCEAS/OHI
### R script to create tables for data
### Melanie Frazier Mar 6 2014
### NOTE: Forgot I had done this and used the analysis here:
### N:\git-annex\Global\FigurePrep_2013OHI
### (but for future- would be good to update from here)
#######################################################################
# ##NOTE the 2014-10-21 and 2014-10-22 data are the same:
# data21 <- read.csv('global2014/scores_Radical_2014-10-21.csv') %>%
#   select(scenario, goal, dimension, region_id, value21=value)
#   
# data22 <- read.csv('global2014/scores_Radical_2014-10-22.csv') %>%
#   select(scenario, goal, dimension, region_id, value22=value) %>%
#   left_join(data21) %>%
#   mutate(diff=value22-value21)
# summary(data22)  

## Downloading packages
library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)
library(colorspace)
library(RColorBrewer)

source("http://nceas.ucsb.edu/~frazier/myTheme.txt")
source("http://nceas.ucsb.edu/~frazier/functions/sort.txt")
#sort.data.frame
#write.excel
#read.excel
save_dir <- "N:/git-annex/Global/FigurePrep_2013OHI"

#####################
## Creating table S27
#### OHI 2013 summary data:

OHIscores2012 <- read.csv("C:/Users/Melanie/Github/ohi-global/eez2012/scores2012_Oct22_2014.csv", 
                          stringsAsFactors=FALSE)  
OHIscores2013 <- read.csv("C:/Users/Melanie/Github/ohi-global/eez2013/scores2013_Oct22_2014.csv",
                          stringsAsFactors=FALSE)
OHIscores2012$scenario <- "2012"  
OHIscores2013$scenario <- "2013"  
OHIscores <- rbind(OHIscores2012, OHIscores2013)

# region names and areas:
rgnNames <- read.csv("N:/model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv", 
                     stringsAsFactors=FALSE)  
rgnAreas <- read.csv("N:/model/GL-NCEAS-OceanRegions_v2013a/data/rgn_areas.csv", 
                     stringsAsFactors=FALSE)  


DataSummary <- function(Measure="score", Date="2013", round=0){
# Measure = "score"
# Date = "2013"
# round = 0

# getting data in correct format:
# join with region name
rgnNames <- rgnNames %>%
  select(region_id=rgn_id, rgn_nam)

scores <- OHIscores %>% 
  filter(dimension %in% Measure) %>%
  filter(scenario %in% Date) %>%
  spread(goal, score) %>%
  left_join(rgnNames, by="region_id") %>%
  arrange(region_id) %>%
  select(code=region_id, "Country/EEZ"=rgn_nam, Index, FIS, FP, MAR,
         AO, NP, CS, CP, LIV, LE, ECO, TR, ICO, SP,
         LSP, CW, HAB, BD, SPP) 

scores$"Country/EEZ"[scores$code==0] <- "Global (area-weighted average)"
scores$code[scores$code==0] <- NA
scores[, 3:21] <- apply(scores[, 3:21], 2, function(x)round(x, round))
scores
}

scores2013 <- DataSummary(Measure="score", Date="2013", round=0)
#write.csv(scores2013, "FigurePrep_2013OHI/Data2013_round0.csv", row.names=FALSE, na="")
scores2012 <- DataSummary(Measure="score", Date="2012", round=0)
#write.csv(scores2012, "FigurePrep_2013OHI/Data2012_round0.csv", row.names=FALSE, na="")


#formatting in word:
# Open in Excel, replace NA with "" (be sure to avoid country names!)
# Delete existing values
# Copy and paste into word (paste option: Merge Table (M))
# Calibri 8
# Center numbers: layout tab
# Center right align region names
# select data cells: layout tab row height =0.1
# to make from scratch:
# Page Layout, Orientation, Landscape
# Table Design select gray/white
# Calibri size 8 font
# Right click: Autofit contents
# unbold column and row names
# center text
# Rowheight = 0.1 width = 0.38 (all except header)
# Copy header from previous files
# Repeat table heading: select header, Table Tools, Layout, DAta, click Repeat header rows

mean(scoresCast2013$Index[-(1)], na.rm=TRUE)
min(scoresCast2013$Index[-(1)], na.rm=TRUE)
max(scoresCast2013$Index[-(1)], na.rm=TRUE)
length(scoresCast2013$Index[-(1)])
sort(scoresCast2013, by=~-Index)[1:13,]
sort(scoresCast2013, by=~Index)[1:13,]
scoresCast2013[grep("South Georgia", scoresCast2013$"Country/EEZ"),]
scoresCast2013[grep("Suriname", scoresCast2013$"Country/EEZ"),]
scoresCast2013[grep("Tuvalu", scoresCast2013$"Country/EEZ"),]
scoresCast2013[grep("Seychelles", scoresCast2013$"Country/EEZ"),]
scoresCast2013[grep("Netherlands", scoresCast2013$"Country/EEZ"),]
scoresCast2013[grep("Croatia", scoresCast2013$"Country/EEZ"),]
scoresCast2013[grep("France", scoresCast2013$"Country/EEZ"),]

###################################################
## Difference between 2012 and 2013:
###################################################
scores2012_round1 <- DataSummary(Measure="score", Date="2012", round=1)
scores2013_round1 <- DataSummary(Measure="score", Date="2013", round=1)

#check to make sure the data are consistent
scores2012_round1$code==scores2013_round1$code #should all be true

#determine difference 
scoresDiff <- scores2013_round1[ ,3:21] - scores2012_round1[ ,3:21]

scoresDiff2 <- as.matrix(scoresDiff)
mean(scoresDiff2, na.rm=TRUE)

sums <- data.frame(apply(scoresDiff2, 1, sum, na.rm=TRUE))
names(sums)[1] <- "sumScores"
ggplot(sums, aes(x=sumScores))+
  geom_histogram(fill="gray", colour="black")+
#  myTheme +
  labs(x="Sum of score differences (2013-2012)", y="Number of countries")
#ggsave("C:\\Users\\Melanie\\Desktop\\BenTasksDec52013\\Hist of Diffs.png")

scoresDiff <- cbind(scores2012[,1:2], scoresDiff)
write.csv(scoresDiff, "TableDiffs2013_2012.csv", row.names=FALSE, na="")
# To add to an existing table in Word (from Excel):
# paste option merge with table
# For whatever reason: I don't think the following works anymore:
# In the first column you can only right click and paste rows. 
# In the second column you can right click and paste cells. 
# So, right click in first column and insert column to left. 
# Then right click in the second column (the original first column) and paste cells. 
# Then right click in first column, delete cells, delete entire column.
# paste option second one


mean(as.numeric(scoresDiff[1,-(1:3)]))
mean(scoresDiff$Index[-(1)], na.rm=TRUE)
sd(scoresDiff$Index[-(1)], na.rm=TRUE)
min(scoresDiff$Index[-(1)], na.rm=TRUE)
max(scoresDiff$Index[-(1)], na.rm=TRUE)
length(scoresDiff$Index[-(1:2)])
apply(scoresDiff[-(1), -(1:2)], 2, mean, na.rm=TRUE)

sort(scoresDiff, by=~-Index)[1:13,]
sort(scoresDiff, by=~Index)[1:13,]


(80-71)/71 #South Georgia and South Sandwich Islands
(65-59)/65 #Suriname

scoresDiff[grep("Tuvalu", scoresDiff$"Country/EEZ"),]
scoresDiff[grep("Georgia", scoresDiff$"Country/EEZ"),]
scoresDiff[grep("Polynesia", scoresDiff$"Country/EEZ"),]
scoresDiff[grep("Wallis", scoresDiff$"Country/EEZ"),]
scoresDiff[grep("Qatar", scoresDiff$"Country/EEZ"),]
scoresDiff[grep("Norway", scoresDiff$"Country/EEZ"),]
scoresDiff[grep("Suriname", scoresDiff$"Country/EEZ"),]
scoresDiff[grep("Guatemala", scoresDiff$"Country/EEZ"),]
scoresDiff[grep("Bulgaria", scoresDiff$"Country/EEZ"),]

subset(scoresDiff,scoresDiff$"Country/EEZ" %in% c("South Georgia and the South Sandwich Islands",
                                                 "Wallis and Futuna",
                                                 "Egypt",
                                                 "Northern Mariana Islands and Guam",
                                                 "Algeria",
                                                 "Antigua and Barbuda",
                                                 "Gambia",
                                                 "Cyprus",
                                                 "Bulgaria",
                                                 "Suriname"))


### comparison of 2012 and 2013 data

eez2012_2013  <- OHIscores2012 %>%
  left_join(OHIscores2013, by=c('goal', 'dimension', 'region_id')) %>%
  select(goal, dimension, region_id, eez2012=score.x, eez2013=score.y)

eez2012_2013$goal <- factor(eez2012_2013$goal, 
                            levels=c('AO', 'BD', 'CP', 'CS', 'CW', 'FP', 'LE', 'NP', 'SP', 
                                     'TR', 'SPP', 'HAB', 'FIS', 'MAR', 'LIV', 'ECO', 'ICO', 'LSP', 'Index'))
eez2012_2013$dimension <- factor(eez2012_2013$dimension, 
                                 levels=c('score', 'status', 'future', 'trend', 'pressures', 'resilience'))

ggplot(subset(eez2012_2013, dimension %in% c("future", 'pressures', "resilience", "score", "status")), 
       aes(x=eez2012, y=eez2013)) + 
  geom_point() +
  #  stat_smooth(method=lm) +
  geom_abline(slope=1, intercept=0, color="red")+
  facet_grid(goal~dimension) +
  theme_bw()
#ggsave("eez2012vs2013.png")


######################################################
### Some models
#######################################################
# data for Human Development Index
# developed countries = 1
# developing = 0
# sovereign regions get parent value
# all other regions get 0

## This HDI indicates only whether it is developed or undeveloped:
#connection <- getURL("sftp://neptune.nceas.ucsb.edu/var/data/ohi/model/GL-UNDP-HDI_v2012/data/rgn_undp_hdi.csv", 
#                     userpwd=password)
#hdi <- read.csv(textConnection(connection))  
#hdi <- rename(hdi, c(rgn_id_2013="code", boolean="hdi"))
#hdi[hdi$code=="209",] # 2 records for China, will keep the main one
#hdi <- hdi[hdi$rgn_nam_2013 != "Hong Kong China (SAR)", ]

## Redid this metric to be continuous using the following script:
## C:\Users\Melanie\Desktop\OHI 2013 Review\GL-UNDP-HDI_v2012\clean_UNDP_mrf.R
hdi <- read.csv("C:\\Users\\Melanie\\Desktop\\NCEAS\\Projects\\OHI 2013\\GL-UNDP-HDI_v2012\\data\\HDI_mrf.csv")  
hdi <- rename(hdi, c(rgn_id_2013="code", HDI="hdi"))

# countries to regions

cntry_rgn <- read.csv('eez2014/layers/cntry_rgn.csv', stringsAsFactors=FALSE)
rgn_labels <- read.csv('eez2014/layers/rgn_labels.csv', stringsAsFactors=FALSE) %>%
  select(rgn_id, rgn_name=label)

cntry_rgn = cntry_rgn %>%
  select(rgn_id, cntry_key) %>%
  left_join(rgn_labels, by="rgn_id") %>%
  arrange(rgn_name, cntry_key) %>%
  select(rgn_id, rgn_name, cntry_key)

cntry_rgn = cntry_rgn %>%
  mutate(
    cntry_key = revalue(cntry_key, c(
      'SCG'            = 'MNE',  # used to be Serbia (no coast) and Montenegro (has coast) in Nature 2012
      'Aruba'          = 'ABW',  # ABW and ANT EEZs got split...
      'Bonaire'        = 'ANT',
      'Curacao'        = 'ANT',
      'Sint Eustatius' = 'ANT',
      'Saba'           = 'ANT',
      'Brunei'         = 'BRN',  # Brunei new country in Malaysia
      'Malaysia'       = 'MYS'))) %>%
  rbind_list(
    data.frame(rgn_id=221, rgn_name='Northern Saint-Martin', cntry_key='BLM'),  # BLM is Saint Barthélemy, included within Northern Saint-Martin (MAF)
    data.frame(rgn_id=209, rgn_name=                'China', cntry_key='HKG'),  # add Hong Kong to China (CHN)
    data.frame(rgn_id=209, rgn_name=                'China', cntry_key='MAC'))  # add Macau to China (CHN)

cntry_landlocked = c('BDI','BOL','BWA','CHE','LUX','MKD','MWI','PRY','PSE','SCG','SVK','SWZ','TKM','UGA','ZMB')

# data for GDP
# GDP = Gross Domestic Product (current $USD)
gdp <- read.csv("eez2013/layers/le_gdp.csv") 

gdp <- gdp %>%
  left_join(cntry_rgn, by=('cntry_key')) %>%
  filter(!(cntry_key %in% cntry_landlocked)) %>%
  group_by(year, rgn_id) %>%
  summarize(usd=sum(usd)) %>%
  select(rgn_id, year, usd)

gdp2010 <-  gdp %>%
  filter(year==2010)
gdp2011 <-  gdp %>%
  filter(year==2011)
use2010 <- setdiff(gdp2010$cntry_key, gdp2011$cntry_key)

table(gdp$year)
gdp <- gdp[gdp$year=="2011", ] #choosing most recent year
gdp <- rename(gdp, c(rgn_id="code", USD="gdp_USD"))
gdp[gdp$code=="209", ] #3 for china, will sum
gdp <- ddply(gdp, .(code, year), summarize, gdp_USD=sum(gdp_USD) )

# PPP = Purchase power parity
gdp_adj <- read.csv("N:/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_gdppcppp_2013a.csv")  
table(gdp_adj$year)
gdp_adj <- gdp_adj[gdp_adj$year=="2012", ] #choosing most recent year
gdp_adj <- rename(gdp_adj, c(rgn_id="code", USD="gdp_adj"))
gdp_adj[gdp_adj$code=="209", ] #3 for china, will sum (need to check on this)
gdp_adj <- ddply(gdp_adj, .(code, year), summarize, gdp_adj=sum(gdp_adj) )
gdp_adj <- gdp_adj[!is.na(gdp_adj$code), ]
# PPPpcGDP = GDP adjusted per capita by PPP

## Population data
pop <- read.csv("N:/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_pop_2013a_updated.csv")
table(pop$year) # I used 2011 because it is the most recent and most complete
pop <- subset(pop, year=="2011")
dupRgns <- pop$rgn_id[duplicated(pop$rgn_id)]
pop[pop$rgn_id %in% dupRgns, ]
pop <- ddply(pop, .(rgn_id), summarise, pop=sum(count))
pop <- pop[!is.na(pop$rgn_id), ]
pop <- rename(pop, c(rgn_id="code"))

## analysis
scoresCast2013 <- join(scoresCast2013, hdi, by="code")
scoresCast2013 <- join(scoresCast2013, gdp, by="code")
scoresCast2013 <- join(scoresCast2013, gdp_adj, by="code")
scoresCast2013 <- join(scoresCast2013, pop, by="code")

# remove the averages across countries:
scoresCast2013_2 <- scoresCast2013[-(1), ]
#write.csv(scoresCast2013_2, "scoresCast2013_2_analysis.csv", row.names=FALSE)

pairsData <- subset(scoresCast2013_2, select=c(hdi, gdp_USD, gdp_adj, pop))
pairsData <- data.frame(log(pairsData[,2:4]), hdi=pairsData$hdi)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="na.or.complete"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r*1)
}

pairs(pairsData, lower.panel=panel.smooth, upper.panel=panel.cor)

# model selection:
scoresCast2013_3 <- subset(scoresCast2013_2, select=c(code, rgn_nam_2013,
                                                      Index, hdi, gdp_USD,
                                                      pop, gdp_adj))
scoresCast2013_3 <- na.omit(scoresCast2013_3)
dim(scoresCast2013_3)
mod1 <- lm(Index ~ hdi, data=scoresCast2013_3)
mod2 <- lm(Index ~ log(gdp_USD), data=scoresCast2013_3)
mod3 <- lm(Index ~ log(gdp_adj), data=scoresCast2013_3)
mod4 <- lm(Index ~ log(pop), data=scoresCast2013_3)

mod5 <- lm(Index ~ hdi + log(pop), data=scoresCast2013_3)
mod6 <- lm(Index ~ hdi + log(gdp_USD), data=scoresCast2013_3)
mod7 <- lm(Index ~ hdi + log(gdp_adj), data=scoresCast2013_3)
mod8 <- lm(Index ~ log(pop) + log(gdp_USD), data=scoresCast2013_3)
mod9 <- lm(Index ~ log(pop) + log(gdp_adj), data=scoresCast2013_3)
mod10 <- lm(Index ~ log(gdp_USD) + log(gdp_adj), data=scoresCast2013_3)

mod11 <- lm(Index ~ hdi + log(pop) + log(gdp_USD), data=scoresCast2013_3)
mod12 <- lm(Index ~ hdi + log(pop) + log(gdp_adj), data=scoresCast2013_3)
mod13 <- lm(Index ~ hdi + log(gdp_USD) + log(gdp_adj), data=scoresCast2013_3)
mod14 <- lm(Index ~ log(pop) + log(gdp_USD) + log(gdp_adj), data=scoresCast2013_3)

mod15 <- lm(Index ~ hdi + log(pop) + log(gdp_USD) + log(gdp_adj), data=scoresCast2013_3)

BIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, 
    mod9, mod10, mod11, mod12, mod13, mod14, mod15)

# use all data for full model:
mod1b <- lm(Index ~ hdi, data=scoresCast2013_2)
summary(mod1b)

ggplot(scoresCast2013_2, aes(x=hdi, y=Index))+
  myTheme+
  geom_point(size=2.5)+
  labs(y="Index score", x="Human Development Index") +
  stat_smooth(method=lm)+  
  coord_cartesian(ylim=c(39, 101))
ggsave("figs\\HDI.png", width=7, height=6, dpi=300)

ggplot(scoresCast2013_2, aes(x=log(pop), y=Index))+
  myTheme+
  geom_point(size=2.5)+
  labs(y="", x="ln population") +
  #stat_smooth(method=lm)  +
  coord_cartesian(ylim=c(39, 101))
ggsave("figs\\pop.png", width=7, height=6, dpi=300)


ggplot(scoresCast2013_2, aes(x=log(gdp_USD), y=Index))+
  myTheme+
  geom_point(size=2.5)+
  labs(y="", x="ln GDP, USD")+
  coord_cartesian(ylim=c(39, 101))
ggsave("figs\\GDP.png", width=7, height=6, dpi=300)



##################  Comparing indicators ##############################

## Pairwise comparison of goal scores:
scoresCast2013_2main <- subset(scoresCast2013_2, select=c("FP", "AO", "NP", "CS", "CP", "LE", "TR", "SP", "CW", "BD"))
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="na.or.complete"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = (cex.cor * r + .15)*3)
}

pairs(scoresCast2013_2main,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=16, cex=.5)
## export as pdf with 7x7 dimensions


#seeing ones were significant
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
pairs(scoresCast2013_2main,
      lower.panel=panel.smooth, upper.panel=panel.cor)

mod <- lm(FP ~ NP, scoresCast2013_2main)
summary(mod)
mod <- lm(AO ~ CS, scoresCast2013_2main)
summary(mod)
mod <- lm(SP ~ NP, scoresCast2013_2main)
summary(mod)
mod <- lm(BD ~ AO, scoresCast2013_2main)
summary(mod)
mod <- lm(CW ~ SP, scoresCast2013_2main)
summary(mod)
mod <- lm(CW ~ TR, scoresCast2013_2main)
summary(mod)
mod <- lm(CW ~ CP, scoresCast2013_2main)
summary(mod)
mod <- lm(CW ~ BD, scoresCast2013_2main)
summary(mod)
