# Various information about ohi-global

### Creating a new scenario (e.g., eez2012, highseas2015, antarctica2014)
1. Copy the folder of a similar scenario and change the year to the scenario year
2. Update the layers_*region*.csv file with the file paths and names of the data for each scenario (see below for variable metadata) 
3. Update following files within the scenario conf folder: goals.csv (scenario years, etc.); pressures_matrix.csv, resilience_matrix.csv, resilience_weights.csv
4. Update the calculate_scores_all.R as follows:
 - Add the "scenario" to the scenario list (we might want to move this information to a table at some point):
 
```
 # scenario list (need to add new scenarios here)
scenarios = list(
  eez2015     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2015a',
    fld_fn       = 'fn_2015a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T),  
  eez2014     = list(
    layer   = 'layers_eez',
  
```

 - Add the scenarios to the "sync" functions.  This overwrites any changes to the function files with one version so that the functions are consistent across years:

```
 ### sync functions.R: 
# overwrite eez2012, eez2014, eez2015, with eez2013
for (dir in c('eez2012','eez2014', 'eez2015')){
  stopifnot(file.copy('eez2013/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}
# overwrite antarctica2015 with antarctica2014
for (dir in c('antarctica2015')){
  stopifnot(file.copy('antarctica2014/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}
# overwrite highseas2015 with highseas2014
for (dir in c('highseas2015')){
  stopifnot(file.copy('highseas2014/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}

```

### Metadata for layers_*region*.csv files (e.g., layers_eez.csv)

The layers files for some of the regions do not necessarily include all these variables. Additional variables can be added for record keeping or metadata purposes.  This will not effect the function of the toolbox.

variable   |   example    | description    
---------- | ------------ | -------------
targets    | AO, pressures | goal/s, pressure, resilience the layer is used to calculate
layer      | ao_access, fis_b_bmsy | internal toolbox name used for layer; NOTE: changes to these names will require changing functions.R
dir_*year*a | ohiprep:Global/WorldBank-Statistics_v2012/data | directory location of layer file; "neptune_data" indicates directory is on neptune server; "ohiprep" is located on [Github](https://github.com/OHI-Science/ohiprep)
fn_*year*a   | r_mora_s4_2013a.csv, mean_catch.csv | filename of .csv layer
name  | Fisheries management effectiveness and opportunity | descriptive name
decription | ... | longer description of data with related references
fld_value | trend, b_bmsy | This should correspond to a variable name in the .csv file with the data (if this variable name changes in the .csv file, it should be changed here)
units | value, km^2 | description of the units of the fld_value
src_*year*a | N, Y | whether the data was updated for a particular scenario year, this isn't used by the toolbox, but it helps us keep updated records
dir_gap_fill_*year*  | ohiprep:Global/FAO-CW-Trends_v2011/data | directory of a file describing where/how missing data was gap-filled
fn_gap_fill_*year*  | rgn_cw_fertilizers_trends_2013a.csv | file describing where/how missing data was gap-filled
clip_n_ship_disag, clip_n_ship_disag_description, uninhabited_expect | ... | information used to create web applications  



### Reporting for OHI 2015 Global:
https://rawgit.com/OHI-Science/ohi-global/draft/global2015/Reporting/Reporting.html
(it may take a while)

### Information used to update data for 2015 analysis
"Preparing for 2015" info placed into issue # 515 for reference...
