library(dplyr)
food_gas_data=read.csv('rawdata.Canada-CHONe2014/AN/aboriginal_food_gas.csv')
ice_layer=read.csv("rawdata.Canada-CHONe2014/AN/ice_layer.csv")

#set 1979 baselines for each community
ice_layer$pC_base=0
food_gas_data$gas_base=0
food_gas_data$food_base=0

ice_layer=arrange(ice_layer,Year,Type,Name)
food_gas_data=arrange(food_gas_data,year,type,name)
l=length(unique(paste(ice_layer$Name,ice_layer$Type)))
for(i in 1:l){
  index=ice_layer$Name==ice_layer$Name[i]&ice_layer$Type==ice_layer$Type[i]
  ice_layer$pC_base[index]=ice_layer$pCover[index&ice_layer$Year==1979]
  index=as.character(food_gas_data$name)==as.character(food_gas_data$name[i])&as.character(food_gas_data$type)==as.character(food_gas_data$type[i])
  food_gas_data$gas_base[index]=food_gas_data$gas[index&food_gas_data$year==1979]
  food_gas_data$food_base[index]=food_gas_data$est_RFNB[index&food_gas_data$year==1979]
}


#ice index
ice_layer$ice_index=ice_layer$pCover/ice_layer$pC_base
ice_layer$ice_index[is.na(ice_layer$ice_index)]=1
ice_layer$ice_index[ice_layer$ice_index>2]=1


#food to gas ratio/fgr_bas*pCover/pC_base
ANindex=(food_gas_data$est_RFNB/food_gas_data$gas)/(food_gas_data$food_base/food_gas_data$gas_base)*(ice_layer$ice_index)
Year=1979:2013
ANmean=rep(0,length(Year))


for(i in 1:l){
  Aboriginal_Needs_Index=ANindex[ice_layer$Name==ice_layer$Name[i]&ice_layer$Type==ice_layer$Type[i]]
  ANmean=ANmean+(Aboriginal_Needs_Index*food_gas_data$population[food_gas_data$name==food_gas_data$name[i]&food_gas_data$type==food_gas_data$type[i]])/sum(food_gas_data$population[1:l])
}
ANmean2=data.frame(cbind(rgn_id=rep(1:278,each = length(Year)),year=rep(Year,278),value=rep(0.5,278*length(Year))))
ANmean2$value[ANmean2$rgn_id==218]=ANmean
write.csv(ANmean2,'rawdata.Canada-CHONe2014/AN/AN_timeseries.csv',row.names=F)
