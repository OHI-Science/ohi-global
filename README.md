Preparing for 2015
============
Here is where we are (I still need to add resilience).  

Here is a [link](https://docs.google.com/a/nceas.ucsb.edu/spreadsheets/d/13rYgPYu9PgqsRTpkXikqVuDtdKZUN5Vs1yOsP5Do5ys/edit?usp=sharing) to a summary of each data layer so far.

###Working on now:

* **Prepare ohi-global for 2015** I will do this soon so we can start looking at the outputs when we add new data.  Have started thinking about [workflow](https://github.com/OHI-Science/issues/issues/409).

* **Summarizing the information**  Still need to review the resilience measures.  I also need to look through issues and "new data" references.  As I go through the data, I will also record instances of gap-filling in this [issue](https://github.com/OHI-Science/issues/issues/351).

* **Pressures: po_trash** Jamie has briefly looked into these data, and we need to develop a method of summarizing them.  Data is located here: N:\git-annex\globalprep\FiveGyres_MarinePlastics_CW\v2015.  We will probably want to get the normal trash data anyway so we can estimate trend (not updated yet).

* **Pressure: ss_wgi** The data is available to update this resilience/pressure data.  Julie is working on this! Go Julie Go!!

* **LSP** Casey has updated these data [Issue 355](https://github.com/OHI-Science/issues/issues/355) and [Issue 387](https://github.com/OHI-Science/issues/issues/387), I need to finalize data.  I also need to update toolbox function to calculate status/trend (mostly to get away from reshape functions).  Casey is interested in ultimately migrating the arcGIS steps into R or other open software (I will put this in future ideas). 

* **Pressure: General** Working with Jamie, Katie, and Ben to determine a consistent strategy for dealing with pressure data.  Particularly transformations and rescaling.  See this [shinyapp](https://jafflerbach.shinyapps.io/decision_tree/).  This ["flowchart"](https://www.lucidchart.com/documents/edit/43e002ea-de05-483c-9569-20eb3a4d44d4?driveId=0ALcIhYsFwBeNUk9PVA) and this [issue](https://github.com/OHI-Science/issues/issues/389).

* **Pressure: Acid** Jamie is getting these spatial data together.  She is preparing an issue for rescaling the data based on thresholds!!  This is very exciting.  

* **Pressure: UV** New data is [available](http://disc.sci.gsfc.nasa.gov/data-holdings/PIP/erythemal_uv_irradiance.shtml). Jamie and I figured out how this was calculated in the past.  We need to determine whether we want to use the anomolies or change in anomolies for pressures (see this [issue](https://github.com/OHI-Science/issues/issues/377)).

* **Pressure: SLR** Jamie has been working on these data.  We have been discussing how to rescale these data (see this [issue](https://github.com/OHI-Science/issues/issues/374)). The final decision is to use the 99.99th quantile.  Also, this is a layer that will be updated for this year, but the data are aggregated across years so there will be no multi-year data. Jamie explored using a different SLR dataset, but this didn't go anywhere:
 
> I recently received this response from the lead author on the paper, and attached is the data sent to me. It seems that their spatialized estimates of historical sea level rise are currently being developed for publication. I've followed up asking if they have any of it ready to share. I'm not sure we can do much with the data provided here except compare these global mean sea level estimates with the dataset we use, but we won't get at any spatial differences...

> I have a feeling we won't get the spatialized data until it is published but will update with any new info.
Conclusion: use the data that she extracted and see if this is available in 2016 (check with: Hay, Carling <carlinghay@fas.harvard.edu>).  

* **NP** New harvest data is available.  Blast and cyanide data can't be updated.  Casey has been working on the script and developing the gap-filling.  I think we will want to rethink how we standardize some of the data layers (i.e., standardizing tonnes by max $ year rather than max ton year).  We will want to rewrite the toolbox function so that it uses all the data that is called.  Also, check on effects of using the running mean - seemed to generate weird results to me - but maybe the results are just weird.  Here is an [issue](https://github.com/OHI-Science/issues/issues/370) describing the function and data that is read by the Toolbox. Here is another NP [issues](https://github.com/OHI-Science/issues/issues/397).

* **LE**  Is it better to have someone translate this from SQL to R?  Or, is it better to spend a few days seeing if we can just write this model from the ground up?

###To do soon:
* **gap-filling script** We are interested in adding a gap-filling across years component (https://github.com/OHI-Science/issues/issues/139).  Have some gap-filling functions that we developed for NP (see this [issue](https://github.com/OHI-Science/issues/issues/397)).

* **CW** This will be updated with new pressures data for fertilizer and pesticide from John's plume models (see [issue](https://github.com/OHI-Science/issues/issues/343)), and the trend will be calculated directly from these pressures.  There is a new trash layer (that I haven't had a chance to look at - but this needs to be done). There is currently no new [pathogen](http://www.wssinfo.org/data-estimates/table) data available.  Will replace population trend with trash trend data as there are now at least three years of trash data!

* **HAB CS CP** Sea ice is probably the only habitat we can update.  These data are ready and available.  These data should be reorganized such that the habitat layers are called separately by the Toolbox.  Although here is a link to some potential [mangrove data](http://www.wri.org/blog/2015/02/satellite-data-reveals-state-world%E2%80%99s-mangrove-forests).  We need to check that out.

* **ICO** New data are available.  Rethink this calculation?  For HS and Antarctica we ran the SPP model with the subset of species that were iconic.

* **SPP** Updates to Aquamaps and IUCN. How did we get Aquamaps data in the past (did we have to request it)? We might be able to download Aquamaps data on a cell by cell basis (see details on spreadsheet).  This might be a challenging one to update because it is written in Python.  But we can at least figure out what has been updated and download those data.  Some discussion of whether to alter this model (issue #366).  At this point, we are going to calculate this goal using the original method and the method that is used by ICO (i.e., averaging IUCN of species located within eez - no area weighting).

* **Pressure: SST** Data is available for new year.  Might be worth extracting other years.  (check on emails Feb 10 2014 - discussion of how data were analyzed)

* **Pressure: hd_intertidal** This is coastal population 10 miles inland (a proxy for intertidal pressures). Predicted data for subsequent years is available.  We used v3 which is the most recent data available on the website.  The analysis seems very complicated.  Not sure why it was done this way: 
> The first weird thing is that the data are extracted by tmp/rgn_offshore_5km_mol.tif which seems weird.  Also, I should take a look at neptune/model/GL-NCEAS-Pressures_v2013a/tmp/impact_layers_2013_redo/coastal_population/processed_rasters/coastal_boundary_population_sum_2010_buffer_5000m_trans.tif to get a better idea of what is going on.  There is also more description in this file (although I don't fully understand this): "GL-NCEAS-Pressures_Intertidal: We projected the 2000 population density data (GPWv3 2005) into Mollweide, and then marked the edge pixels at 4.224 km resolution. To account for conflation, we extracted all edge pixels that fell within a 10km neighborhood (~2 pixels inland or offshore) referenced by our coastline model at 1km resolution. After extracting the population density edge pixels, we rescaled them using log-plus1 score max and 10 percent plus the global maximum within the edge pixels (1.1 * 122,135). We then grew our OHI regions raster inland 20 pixels, and then extracted the OHI regions using the same 10km neighborhood and 4.224 km resolution. Finally, we computed zonal statistics and use the mean as the pressures score per region." 

The CHI pressures data was used - which is 5000 m buffer and 2006 data for 2012 analysis and 2010 data for 2013 analysis.

Seems like it would be better to use these data: N:\model\GL-NCEAS-CoastalPopulation_v2013\data?  And a 25 km inland area (given that we have that buffer)?  

* **MAR** We have data to update coastal population to 2015 (Population data is projected from <=2000 data - but that may be the best available?).  The FAO harvest tonnes has been updated with 2013 [data](http://www.fao.org/fishery/statistics/global-aquaculture-production/en)).

* **Pressure: target harvest** It appears that the last analysis included FAO data to 2012. Data is located here: N:\git-annex\Global\NCEAS-SpatialFishCatch_v2014\raw\fao\FAO_raw_1950_2012.csv .  Good readme: Github\ohiprep\Global\FAO-TargetedHarvest_v2012.  FAO data has been updated.


###Things to keep checking on:
* **FIS** May have updated catch data.  May also have an updated CMSY model.

* **AO** The new ppppcgdp data is not yet available.

* **FP** Update relative FIS to MAR when updates to these data.

* **CW** This will be updated with new pressures data for fertilizer and pesticide from John's plume models (see [issue](https://github.com/OHI-Science/issues/issues/343)), and the trend will be calculated directly from these pressures.  There is a new trash layer (that I haven't had a chance to look at - but this needs to be done). There is currently no new [pathogen](http://www.wssinfo.org/data-estimates/table) data available.

* **Pressure: Artisanal low bycatch** "Modeled least destructive commercial fishing practices by 2 different gear types". We can probably update this when we get new fishing data, although it depends on the data.  NOTE: the fp_art_lb_2012_NEW.csv and fp_art_lb_2013_NEW.csv are the same data.

* **Pressure: Commercial high bycatch** Sum of pelagic high bycatch and demersal non-destructive high bycatch and demersal high bycatch pressures.  For CHI this is calculated with FAO fishing data in 1999 to 2003 and 2009 to 2011.  There are now 2012 FAO data.  May be new SAUP data coming.  Will evaluate whether it is worth it to update these.

* **Pressure: Commercial low bycatch** Sum of pelagic low bycatch and demersal non-destructive low bycatch pressures?  For CHI this is calculated with FAO fishing data in 1999 to 2003 and 2009 to 2011.  There are now 2012 data.  May be new SAUP data coming.  Will evaluate whether it is worth it to update these.

* **Pressure: hd_subtidal_sb:**  Ben B. had recommended not using "subtidal" because it was calculated using subtidal and soft shelf habitats (for HS we also included soft slope).  This is calculated at the region scale by summarizing the area of soft-bottom and relative proportion of destructive fishing for each eez/fao/etc and then applying the following [method](https://github.com/OHI-Science/ohiprep/blob/master/Global/HS_AQ_Pressures_HD_SB_2014/PressuresHD_subtidal_soft_bottom.png).  Questions: 1) should we apply formula at the raster scale? 2) what categories of soft-bottom do we want to include? 3) how do we incorporate new fishing data (will we have the different fishing types...or just assume equivalent increase/decrease per saup region)?

* **Pressure: po_chemicals_3nm:** Updated by ??.  Includes pesticide plume data (new data), inorganic (based on impervious surfaces, and does not appear to have updated data), ocean-based pollution (check that this was the correct data used...not totally clear.  Includes shipping - which is not going to be updated and ports)?  Inorganic pollution based on landcover impervious surfaces (does not appear to be new data from noaa: http://ngdc.noaa.gov/eog/dmsp/download_global_isa.html - there is one from 2010, but this just appears to be a different projection)   

* **Pressure: po_nutrients_3nm:** Updated by ??. FAO fertilizer data.  Based on these files: Y:\mnt\storage\marine_threats\impact_layers_2013_redo\impact_layers\work\land_based\scripts_and_intermediary_layers\[1]_fao\input\faostats_fert.csv it appears that the data John used in his last analysis went to 2010.  The data are now available to 2011.  There also appears to be updated land use data (http://landcover.usgs.gov/global_climatology.php, or http://glcf.umd.edu/data/lc/) which is used in the model to distribute the FAO nutrient data.

* **Pressure: sp_genetic:** Trujillo data from 2008. No updates for genetic escapes, but updates to FAO mariculture data which seems to be integrated in the calculation.  Can't find the script where this is calclulated.

* **Pressure: po_chemicals** John will be updating these data
* **Pressures: po_pathogens** Data not available, will keep checking

###Not Doing
* **Pressure: Artisanal high bycatch** Data from *Reefs at Risk Revisited* study - which hasn't been updated.

* **Pressure: sp_alien:** This was based on the Molnar 2008 paper (data on the MEOW scale) which has not been updated. We will not be updating these data (see this [issue](https://github.com/OHI-Science/issues/issues/388))

* **Update ohidev** get away from using plyr and change %.% to %>%.  Merge ohidev draft into master and make sure there are no changes to scores.  I think Ben B will be doing this soon.  UPDATE: no longer doing this.  We will just use the dev branch.

###Done
* **Organizing pressure reference points** Create a table that indicates the reference points used to rescale the pressures (https://docs.google.com/a/nceas.ucsb.edu/spreadsheets/d/1FMSqD4vBxYTBCsyejFnOdw-MfE_DgGetZ3B0CS8-fto/edit?usp=sharing).
 
* **John P** Ben H. Update: John will be starting May 1st!

* **data visualization app** Thanks to Ben B!  Located [here](https://github.com/OHI-Science/issues/issues/405).


###Future improvements
* **LSP:** Analyze in open source software.

###General Notes
There is FAO fish data for 2012 (most pressures data goes to 2011). So, it is possible to update these pressure layers.  But, Katie says this is not worth it:
>for the fishing pressures, my opinion is it’s not worth redoing with the 2012 catch data we have, since that is split by gear using a very sketchy method. The whole effort of re-calculating just to incorporate an additional year of poor quality information seems not worthwhile. However, I think it’s worth re-doing with the new SAUP data split by gear, if there’s enough time (now that Jamie generated productivity multipliers we’re able to do that!). We’d need to request that they send the data by half-degree cell, though, to be able to match the productivity data - not sure if they’ll agree to that.


