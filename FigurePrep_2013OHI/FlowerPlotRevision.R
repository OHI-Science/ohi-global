#####################################
## Getting the data together
#####################################
library(RCurl)
library(plyr)
library(dplyr)
library(reshape2)
require('RColorBrewer')

source("C:\\Users\\Melanie\\Desktop\\NCEAS\\OHI R scripts\\FlowerPlotFunction.R")
source("http://nceas.ucsb.edu/~frazier/functions/sort.txt")

setwd("N:/git-annex/Global/FigurePrep_2013OHI")


OHIscores2013 <- read.csv("Data2013_round0.csv")  
OHIscores2012 <- read.csv("Data2012_round0.csv")

## function to organize the data:
organizeData <- function(FlowerData){
FlowerData$code[FlowerData$Country.EEZ == "Global (area-weighted average)"] <- 0
#FlowerData <- subset(FlowerData, Country.EEZ!="Global (EEZ average)")
FlowerData <- rename(FlowerData, c(code="rgn_id", Country.EEZ="rgn_nam"))
FlowerData <- subset(FlowerData, select=c(rgn_id, rgn_nam, Index, FIS, MAR, AO, NP,
                                      CS, CP, TR, LIV, ECO, ICO, LSP, CW, HAB, SPP))
FlowerData <- rename(FlowerData, c(NP= "Natural Products",
                               AO= "Artisanal Fishing \n Opportunities",
                               MAR="Mariculture",
                               FIS="Fisheries",
                               SPP="Species",
                               HAB="Habitats",
                               CW="Clean Waters",
                               LSP="Lasting Special \n Places",
                               ICO="Iconic \n Species",
                               ECO="Economies",
                               LIV="Livelihoods",
                               TR= "Tourism & \n Recreation",
                               CP="Coastal \n Protection",
                               CS="Carbon Storage"))}

OHIscores2013 <- organizeData(OHIscores2013)
OHIscores2012 <- organizeData(OHIscores2012)
############################################################################
## Making the plots
############################################################################
col.brks = c(seq(0,100,length.out=11)) 

PlotFunction <- function(data, main=NA){
aster(lengths=unlist(ifelse(is.na(data[4:17]), 100, data[4:17])), 
      widths=c(0.5, 0.5, 1, 1, 1, 1, 1, 0.5, 0.5, 0.5, 0.5, 1, 0.5, 0.5),
      labels= gsub("NA", "-", paste(names(data)[4:17], round(data[4:17],0), sep="\n")), 
      label.offset=0.15,
      label.cex=1,
      fill.col= ifelse(is.na(data[4:17]), 
                       "#d3d3d3", 
                       brewer.pal(10, 'RdYlBu')[cut(as.numeric(data[4:17]), col.brks, labels=1:10, include.lowest=TRUE)]),
      center=round(data[3], 0), 
      main=main, 
      max.length=100,
      disk=0.35, 
      cex=2,
      cex.main=1.5,
      xlim=c(-1.2, 1.2))} 

# ### old version of plots with rainbow labels#####
# PlotFunction <- function(data, main=NA){
#   aster(lengths=unlist(ifelse(is.na(data[4:17]), 100, data[4:17])), 
#         widths=c(0.5, 0.5, 1, 1, 1, 1, 1, 0.5, 0.5, 0.5, 0.5, 1, 0.5, 0.5),
#         labels= gsub("NA", "-", paste(names(data)[4:17], round(data[4:17],0), sep="\n")), 
#         label.offset=0.15,
#         label.cex=1,
#         fill.col= ifelse(is.na(data[4:17]), 
#                          "#d3d3d3", 
#                          colorRampPalette(brewer.pal(11, 'Spectral'), space="Lab")(14)),
#         center=round(data[3], 0), 
#         main=main, 
#         max.length=100,
#         disk=0.35, 
#         cex=2,
#         cex.main=1.5,
#         xlim=c(-1.2, 1.2))} 
# data <- OHIscores2013[OHIscores2013$rgn_id == 0,]
# png(file="figs\\Index_2013_old_colors.png", 
#     res=300, height=8, width=8, units="in")
# par(mfrow=c(1,1))
# PlotFunction(data)
# #par(pty="s") 
# dev.off()


# 2013 index
data <- OHIscores2013[OHIscores2013$rgn_id == 0,]
png(file="figs/Index_2013.png", 
    res=300, height=8, width=8, units="in")
par(mfrow=c(1,1))
PlotFunction(data)
#par(pty="s") 
dev.off()

# 2012 index
data <- OHIscores2012[OHIscores2012$rgn_id == 0,]
png(file="figs\\Index_2012.png", 
    res=300, height=8, width=8, units="in")
par(mfrow=c(1,1))
PlotFunction(data)
#par(pty="s") 
dev.off()
 
##figure out best and worst performing countries in 2013
OHIscores2013_order <- OHIscores2013 %>%
  arrange(Index)
head(OHIscores2013_order)
tail(OHIscores2013_order)

countries <- c("Heard and McDonald Islands", "Canada",
               "United States", "Democratic Republic of the Congo", "China",
               "Liberia", "Russia", "United Kingdom", "Australia",               
               "Guinea Bissau", "Prince Edward Islands", "Howland Island and Baker Island",
               "Sierra Leone", "Angola", "Saint Vincent and the Grenadines")

for(i in 1:length(countries)){
  #i <- 1
  country <- countries[i]
  png(file=paste("figs\\", country, ".png", sep=""), 
      res=300, height=8, width=8, units="in")
  data <- OHIscores2013[OHIscores2013$rgn_nam %in% country,]
  PlotFunction(data, main=country) 
  dev.off()
  }

