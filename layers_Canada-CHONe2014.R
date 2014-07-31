library(plyr)
library(dplyr)

wd = '~/Documents/GitHub/ohi-global/eezCHONE'
#wd = '/Users/bbest/Github_Mac/ohicore_Canada-CHONe/inst/extdata'
setwd(wd)

########################################## add carbon storage habitats #############################################################
pres=read.csv('layers/hab_presence.csv', na.strings='', stringsAsFactors=F);head(pres);
ext=read.csv('layers/hab_extent.csv', na.strings='', stringsAsFactors=F);head(ext);
health=read.csv('layers/hab_health.csv', na.strings='', stringsAsFactors=F);head(health);
trend=read.csv('layers/hab_trend.csv', na.strings='', stringsAsFactors=F);head(trend);

# inject permafrost and clathrates
#presence
newpres=data.frame(cbind(rgn_id=1:278,habitat=rep(c('clathrates','permafrost'),each=278),boolean=0))
newpres$boolean[newpres$rgn_id==218]=1
pres=rbind(pres,newpres)

#extent
ext=rbind(ext,data.frame(cbind(rgn_id=218,habitat=c('clathrates','permafrost'),km2=c(2517375.704,819702.5225))))

# calculate temperature based on CO2
co2 = read.csv('rawdata.Canada-CHONe2014/HAB/CO2.csv')

# set baseline to pre-industrial levels (280 ppm)
co2$anomaly = co2$mean-280

#calculate health
co2$health = 1-co2$anomaly/(550-280)

#health
max_year = max(co2$year)
status = co2$health[which(co2$year==max_year)]
health = rbind(health,data.frame(cbind(rgn_id=218,habitat=c('clathrates','permafrost'),health=c(status,status))))

#trend
d = data.frame(status=co2$health, year=co2$year)[tail(which(!is.na(co2$health)), 20),]
t = lm(status ~ year, d)$coefficients[['year']]
trend = rbind(trend,data.frame(cbind(rgn_id=218,habitat=c('clathrates','permafrost'),trend=c(t,t))))

# write out modified layers
write.csv(pres,'layers/hab_habitat_presence.csv', na='', row.names=F)
write.csv(ext,'layers/hab_habitat_extent.csv', na='', row.names=F)
write.csv(health,'layers/hab_habitat_health_disaggregatedNature2012.csv', na='', row.names=F)
write.csv(trend,'layers/hab_habitat_trend_disaggregatedNature2012.csv', na='', row.names=F)


########################################## Change iconic species #############################################################
# get species 
spp_ico = read.csv('rawdata.Canada-CHONe2014/ICO/iconic_species.csv', stringsAsFactors=F); head(spp_ico)
spp_sara = read.csv('rawdata.Canada-CHONe2014/ICO/SARA_sp.csv', stringsAsFactors=F); head(spp_sara)


# TODO: alter weights to something sensible, like IUCN analogue for status weights

# read in weights
spp_range_weights = read.csv('rawdata.Canada-CHONe2014/ICO/spp_range_weights.csv', stringsAsFactors=F); head(spp_range_weights)

# create weights for species category. Can't just use category because averaging multiple ranges
paste(unique(spp_sara$COSEWIC_Status), collapse="'=0.,'")
spp_status_weights = c('Not Assessed'    = 0,
                       'Non-active'      = 0,
                       'Data Deficient'  = 0,
                       'Not at Risk'     = 0,
                       'Special Concern' = 0.2,
                       'Threatened'      = 0.4,
                       'Endangered'      = 0.6,
                       'Extinct'         = 1,
                       'Extirpated'      = 1)
spp_status_weights = data.frame(COSEWIC_Status = names(spp_status_weights),
                                status_weight  = spp_status_weights, stringsAsFactors=F)

# get status for iconics
spp_ico_sara = spp_ico %.%
  inner_join(spp_sara, by='Scientific_name') %.%
  inner_join(spp_range_weights, by='Range') %.%
  inner_join(spp_status_weights, by='COSEWIC_Status') %.%
  rename(c(Scientific_name='sciname')) %.%
  group_by(sciname) %.%
  summarise(
    value = weighted.mean(x=status_weight, w=range_weight),
    rgn_id        = 218)
  
###
# create new ico_spp_extinction_status now with extinction risk as numeric value and not categorical with these Canada values swapped in
ico_spp_status = read.csv('layers/ico_spp_extinction_status.csv', stringsAsFactors=F); head(ico_spp_status)

# lookup for weights status
w.risk_category = c('LC' = 0,
                    'NT' = 0.2,
                    'VU' = 0.4,
                    'EN' = 0.6,
                    'CR' = 0.8,
                    'EX' = 1)
# set value
ico_spp_status$value = w.risk_category[ico_spp_status$category]; head(ico_spp_status)

# inject Canada values
ico_spp_status = ico_spp_status %.%
  filter(rgn_id != 218) %.%
  select(rgn_id, sciname, value) %.%
  rbind(spp_ico_sara)
  
# write out new layer
write.csv(ico_spp_status, 'layers/ico_spp_extinction_status_value_Canada-CHONe.csv', na='', row.names=F)

# read layers.csv
layers = read.csv('layers.csv', na.strings='', stringsAsFactors=F); head(layers); 

# alter fields for this updated layer
i = which(layers$layer=='rnk_ico_spp_extinction_status_value')
layers$layer[i]     = 'rnk_ico_spp_extinction_status_value'
layers$name[i]      = 'SARA extinction risk value for iconic species'
layers$units[i]     = 'value'
layers$filename[i]  = 'ico_spp_extinction_status_value_Canada-CHONe.csv'
layers$fld_value[i] = 'value'

# write back updated layers.csv
write.csv(layers, 'layers.csv', na='', row.names=F)


########################################## replace WGI with CWI #############################################################

CIW=read.csv('rawdata.Canada-CHONe2014/CIW/CIW-GDP-Domains-1994-2010.csv', stringsAsFactors=F)

# extrapolate CIW and GDP
fitCIW=lm(CIW~Year,data=CIW)
fitGDP=lm(GDP~Year,data=CIW)

# create CIW layers
ciw=data.frame(cbind(rgn_id=1:250,score=0.5))
ciw[218,2]=(coef(fitCIW)[2]*2013+coef(fitCIW)[1])/(coef(fitGDP)[2]*2013+coef(fitGDP)[1])
ciw2=data.frame(cbind(rgn_id=ciw$rgn_id,score=1-ciw$score))

# write out new CIW layers
write.csv(ciw, 'layers/rgn_wb_cwi_2013_rescaled.csv', na='', row.names=F)
write.csv(ciw2, 'layers/rgn_wb_cwi_2013_rescaled_inverse.csv', na='', row.names=F)

# alter fields for this updated layer
layers = read.csv('layers.csv', na.strings='', stringsAsFactors=F); head(layers); 

i = which(layers$layer=='ss_wgi')
layers$layer[i]     = 'ss_cwi'
layers$name[i]      = 'Hardship of Canadians indicated with the CWI'
layers$units[i]     = 'pressure score'
layers$filename[i]  = 'rgn_wb_cwi_2013_rescaled_inverse.csv'
layers$fld_value[i] = 'score'
layers$val_min[i] = 0
layers$val_max[i] = 1


i = which(layers$layer=='wgi_all')
layers$layer[i]     = 'cwi_all'
layers$name[i]      = 'Wellbeing of Canadians indicated with the CWI'
layers$units[i]     = 'resilience score'
layers$filename[i]  = 'rgn_wb_cwi_2013_rescaled.csv'
layers$fld_value[i] = 'score'
layers$val_min[i] = 0
layers$val_max[i] = 1

# write back updated layers.csv
write.csv(layers, 'layers.csv', na='', row.names=F)

# update pressure/resilience matrices and resilience weights
rw=read.csv('conf/resilience_weights.csv', stringsAsFactors=F)
rw$layer[rw$layer=='wgi_all']='cwi_all'
write.csv(rw,'conf/resilience_weights.csv',row.names=FALSE)

pm=read.csv('conf/pressures_matrix.csv', stringsAsFactors=F)
names(pm)[names(pm)=='ss_wgi']='ss_cwi'
write.csv(pm,'conf/pressures_matrix.csv',row.names=FALSE,na="")

rm=read.csv('conf/resilience_matrix.csv', stringsAsFactors=F)
names(rm)[names(rm)=='wgi_all']='cwi_all'
rm$cwi_all='cwi_all'
write.csv(rm,'conf/resilience_matrix.csv',row.names=FALSE)


##################################### Aboriginal Needs ################################################################################
source("rawdata.Canada-CHONe2014/AN/AN_timeseries.R")

# copies modified functions.R in rawdata.Canada-CHONe2014/ and to conf/functions.R 
#file.copy('rawdata.Canada-CHONe2014/functions.R', 'conf/functions.R', overwrite = T)
#file.copy('conf.Global2013.www2013/functions.R', 'conf/functions.R', overwrite = T)

AN = function(layers, 
              Sustainability=1.0){
  print("AN source works")
  layers_data =rename(SelectLayersData(layers, layers='rny_an_timeseries'),c('id_num'='region_id','val_num'='score'))
  #year = 2014
  # status
  r.status = subset(layers_data, year==max(layers_data$year, na.rm=T), c(region_id, score)); summary(r.status); dim(r.status)
  r.status$score = r.status$score * 100 * Sustainability
  
  
  # trend
  r.trend = ddply(
    layers_data, .(region_id), function(x){      
        d = data.frame(status=x$score, year=x$year)[tail(which(!is.na(x$score)), 10),]
        
        data.frame(trend = lm(status ~ year, d)$coefficients[['year']])
      }); # summary(r.trend); summary(subset(scores_www, goal=='AN' & dimension=='trend'))
  
  # return scores
  #browser()
  s.status = cbind(rename(r.status, c('score'='score')), data.frame('dimension'='status')); head(s.status)
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend')); head(s.trend)
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='AN')); dlply(scores, .(dimension), summary)
  return(scores)  
}


# rename Artisinal Opportunities to Aboriginal Needs
goals = read.csv('conf/goals.csv', stringsAsFactors=F); head(goals); 

i = which(goals$goal=='AO')
goals$goal[i]     = 'AN'
goals$name[i]      = 'Aboriginal Needs'
goals$name_flower[i]      = '           Aboriginal Needs'
goals$description[i]     = 'This goal captures The extent to which Canada’s Aboriginals are able to access ocean resources for subsistence, social and ceremonial purposes'
goals$preindex_function[i]  = 'AN(layers)'

# write back updated goals.csv
write.csv(goals, 'conf/goals.csv', na='', row.names=F)

# copy rgn_an_timeseries
file.copy('rawdata.Canada-CHONe2014/AN/AN_timeseries.csv', 'layers/rgn_an_timeseries.csv', overwrite = T)


# alter fields for this updated layer
layers = read.csv('layers.csv', na.strings='', stringsAsFactors=F); head(layers); 

# remove unnecessary layer
i = which(layers$layer!='ao_access')
layers     = layers[i,]

# alter layer
i = which(layers$layer=='ao_need')
layers$targets[i]   = 'AN'
layers$layer[i]     = 'rny_an_timeseries'
layers$name[i]      = 'Timeseries of Aboriginal Needs Status'
layers$filename[i]  = 'rgn_an_timeseries.csv'
layers$val_min[i] = 0
layers$val_max[i] = 1
# write back updated layers.csv
write.csv(layers, 'layers.csv', na='', row.names=F)

pm=read.csv('conf/pressures_matrix.csv', stringsAsFactors=F)
pm$goal[pm$goal=='AO']     = 'AN'
write.csv(pm,'conf/pressures_matrix.csv',row.names=FALSE,na="")

rm=read.csv('conf/resilience_matrix.csv', stringsAsFactors=F)
rm$goal[rm$goal=='AO']     = 'AN'
write.csv(rm,'conf/resilience_matrix.csv',row.names=FALSE)


##################################### weighting ################################################################################

#create function to alter weights
reweigh <- function(w,i){
  goals$weight[goals$goal=='FP']      = w[,names(w)==i][w$X=="FoodProvision"]
  goals$weight[goals$goal=='FIS']     = w[,names(w)==i][w$X=="FoodProvision"]/2
  goals$weight[goals$goal=='MAR']     = w[,names(w)==i][w$X=="FoodProvision"]/2
  goals$weight[goals$goal=='AN']      = w[,names(w)==i][w$X=="AboriginalNeeds"]
  goals$weight[goals$goal=='NP']      = w[,names(w)==i][w$X=="NaturalProducts"]
  goals$weight[goals$goal=='CS']      = w[,names(w)==i][w$X=="CarbonStorage"]
  goals$weight[goals$goal=='CP']      = w[,names(w)==i][w$X=="CoastalProtection"]
  goals$weight[goals$goal=='TR']      = w[,names(w)==i][w$X=="TourismRecreation"]
  goals$weight[goals$goal=='LE']      = w[,names(w)==i][w$X=="CoastalLivelihoods"]
  goals$weight[goals$goal=='LIV']     = w[,names(w)==i][w$X=="CoastalLivelihoods"]/2
  goals$weight[goals$goal=='ECO']     = w[,names(w)==i][w$X=="CoastalLivelihoods"]/2
  goals$weight[goals$goal=='SP']      = w[,names(w)==i][w$X=="IconicPlacesSPecies"]
  goals$weight[goals$goal=='ICO']     = w[,names(w)==i][w$X=="IconicPlacesSPecies"]/2
  goals$weight[goals$goal=='LSP']     = w[,names(w)==i][w$X=="IconicPlacesSPecies"]/2
  goals$weight[goals$goal=='CW']      = w[,names(w)==i][w$X=="CleanWaters"]
  goals$weight[goals$goal=='BD']      = w[,names(w)==i][w$X=="Biodiversity"]
  goals$weight[goals$goal=='HAB']     = w[,names(w)==i][w$X=="Biodiversity"]/2
  goals$weight[goals$goal=='SPP']     = w[,names(w)==i][w$X=="Biodiversity"]/2
  return(goals$weight)
}

