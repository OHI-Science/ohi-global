Preparing for 2015
============
Here is where we are (I have only appraised the goals and about half of the pressures so far).  General note about pressures:
N:\model\GL-NCEAS-Pressures_v2013a (shows that new 2013 pressures data was incorporated into 2014 analysis, I didn't think it had been.)

Here is a [link](https://docs.google.com/a/nceas.ucsb.edu/spreadsheets/d/13rYgPYu9PgqsRTpkXikqVuDtdKZUN5Vs1yOsP5Do5ys/edit?usp=sharing) to a summary of each data layer so far.

###Working on now:
* **

* **Prepare Toolbox for 2015** I will do this soon so we can start looking at the outputs when we add new data.

* **Summarizing the information**  Still need to review the resilience measures.  I also need to look through issues and "new data" references.  As I go through the data, I will also record instances of gap-filling in this [issue](https://github.com/OHI-Science/issues/issues/351).

* **LSP** Updates to these data.  Casey is running the python script [Issue 355](https://github.com/OHI-Science/issues/issues/355).  We have prepared a version of the data.  Casey is going to provide details on the process to determine whether any of the data processing should change.  I notice that the function to calculate status/trend needs to be updated (mostly to get away from reshape functions).  Casey is interested in ultimately migrating the arcGIS steps into R or other open software (I will put this in future ideas). 

* **Pressure: Acid** Jamie is getting these spatial data together.  I have a question about whether there are biological data we could use to scale the data, but since we are using difference data this might be difficult as Jamie has pointed out.

* **Pressures: General** Our pressure layers for 2008 and 2013 are typically aggregated data for several years (e.g., sst=2000-2005 and 2005-2010, etc).  So, is it worth updating these?  If so, what is the best approach? 

* **Pressure: UV** New data is [available](http://disc.sci.gsfc.nasa.gov/data-holdings/PIP/erythemal_uv_irradiance.shtml). Met with Jamie on Friday to discuss this goal.  We figured out how it was calculated in the past and need to determine whether to use change in anomolies or anomolies for baseline (see this [issue](https://github.com/OHI-Science/issues/issues/377).

* **Pressure: SLR** Jamie has been working on these data.  We have been discussing how to rescale these data (see this [issue](https://github.com/OHI-Science/issues/issues/374)), and I think the final decision is to use the 99.99th quantile.  Also, this is a layer that will be updated for this year, but the data are aggregated across years so there will be no multi-year data. Jamie explored using a different SLR dataset, but this didn't go anywhere:
 
> I recently received this response from the lead author on the paper, and attached is the data sent to me. It seems that their spatialized estimates of historical sea level rise are currently being developed for publication. I've followed up asking if they have any of it ready to share. I'm not sure we can do much with the data provided here except compare these global mean sea level estimates with the dataset we use, but we won't get at any spatial differences...

> I have a feeling we won't get the spatialized data until it is published but will update with any new info.
Conclusion: use the data that she extracted and see if this is available in 2016 (check with: Hay, Carling <carlinghay@fas.harvard.edu>).  

###To do soon:
* **ohi-global** Update to include 2015 analysis

* **gap-filling script** Did we want to improve this? (Julie question)

* **data visualization app** This was super handy for comparing changes among commits.

* **Update ohidev** get away from using plyr and change %.% to %>%.  Merge ohidev draft into master and make sure there are no changes to scores.  

* **CW** This will be updated with new pressures data for fertilizer and pesticide from John's plume models (see [issue](https://github.com/OHI-Science/issues/issues/343)), and the trend will be calculated directly from these pressures.  There is a new trash layer (that I haven't had a chance to look at - but this needs to be done). There is currently no new [pathogen](http://www.wssinfo.org/data-estimates/table) data available.

* **HAB CS CP** Sea ice is probably the only habitat we can update.  These data are ready and available.  These data should be reorganized such that the habitat layers are called separately by the Toolbox.  Although here is a link to some potential [mangrove data](http://www.wri.org/blog/2015/02/satellite-data-reveals-state-world%E2%80%99s-mangrove-forests).  We need to check that out.

* **ICO** New data are available.  Rethink this calculation?  For HS and Antarctica we ran the SPP model with the subset of species that were iconic.

* **SPP** Updates to Aquamaps and IUCN. How did we get Aquamaps data in the past (did we have to request it)? We might be able to download Aquamaps data on a cell by cell basis (see details on spreadsheet).  This might be a challenging one to update because it is written in Python.  But we can at least figure out what has been updated and download those data.  Some discussion of whether to alter this model (issue #366).  At this point, we are going to calculate this goal using the original method and the method that is used by ICO (i.e., averaging IUCN of species located within eez - no area weighting).

* **NP** New harvest data is available.  This would be a good one for Casey to practice on (but first redo the gap-filling script?).  Blast and cyanide data can't be updated (as far as I can tell).  Check that the function uses all the data called (I saw a note about that in one of the files I was looking at).  Also, check on effects of using the running mean.  Here is an [issue](https://github.com/OHI-Science/issues/issues/370) describing the function and data that is read by the Toolbox. 

* **LE**  Is it better to have someone translate this from SQL to R?  Or, is it better to spend a few days seeing if we can just write this model from the ground up?

* **Pressure: SST** Data is available for new year.  Might be worth extracting other years.  (check on emails Feb 10 2014 - discussion of how data were analyzed)

* **Pressure: hd_intertidal** This is coastal population 10 miles inland (a proxy for intertidal pressures). Predicted data for subsequent years is available.  We used v3 which is the most recent data available on the website.  The analysis seems very complicated.  Not sure why it was done this way: 
> The first weird thing is that the data are extracted by tmp/rgn_offshore_5km_mol.tif which seems weird.  Also, I should take a look at neptune/model/GL-NCEAS-Pressures_v2013a/tmp/impact_layers_2013_redo/coastal_population/processed_rasters/coastal_boundary_population_sum_2010_buffer_5000m_trans.tif to get a better idea of what is going on.  There is also more description in this file (although I don't fully understand this): "GL-NCEAS-Pressures_Intertidal: We projected the 2000 population density data (GPWv3 2005) into Mollweide, and then marked the edge pixels at 4.224 km resolution. To account for conflation, we extracted all edge pixels that fell within a 10km neighborhood (~2 pixels inland or offshore) referenced by our coastline model at 1km resolution. After extracting the population density edge pixels, we rescaled them using log-plus1 score max and 10 percent plus the global maximum within the edge pixels (1.1 * 122,135). We then grew our OHI regions raster inland 20 pixels, and then extracted the OHI regions using the same 10km neighborhood and 4.224 km resolution. Finally, we computed zonal statistics and use the mean as the pressures score per region." 

The CHI pressures data was used - which is 5000 m buffer and 2006 data for 2012 analysis and 2010 data for 2013 analysis.

Seems like it would be better to use these data: N:\model\GL-NCEAS-CoastalPopulation_v2013\data?  And a 25 km inland area (given that we have that buffer)?  


###Things to keep checking on:
* **FIS** May have updated catch data.  May also have an updated CMSY model.

* **MAR** We have data to update coastal population to 2015 (Population data is projected from <=2000 data - but that may be the best available?).  There hasn't been an update yet for the FAO harvest tonnes (look for addition of 2013 [data](http://www.fao.org/fishery/statistics/global-aquaculture-production/en)).

* **AO** The new ppppcgdp data is not yet available.

* **FP** Update relative FIS to MAR when updates to these data.

* **CW** This will be updated with new pressures data for fertilizer and pesticide from John's plume models (see [issue](https://github.com/OHI-Science/issues/issues/343)), and the trend will be calculated directly from these pressures.  There is a new trash layer (that I haven't had a chance to look at - but this needs to be done). There is currently no new [pathogen](http://www.wssinfo.org/data-estimates/table) data available.

* **Pressure: Artisanal low bycatch** "Modeled least destructive commercial fishing practices by 2 different gear types". We can probably update this when we get new fishing data, although it depends on the data.  NOTE: the fp_art_lb_2012_NEW.csv and fp_art_lb_2013_NEW.csv are the same data.

* **Pressure: target harvest** It appears that the last analysis included FAO data to 2012, but I can't find the original data files to confirm this (asking Julie about this).  Good readme: Github\ohiprep\Global\FAO-TargetedHarvest_v2012.  Assuming data go to 2012 - there are no new data updates.

* **Pressure: Commercial high bycatch** Sum of pelagic high bycatch and demersal non-destructive high bycatch and demersal high bycatch pressures.  For CHI this is calculated with FAO fishing data in 1999 to 2003 and 2009 to 2011.  There are now 2012 FAO data.  May be new SAUP data coming.  Will evaluate whether it is worth it to update these.

* **Pressure: Commercial low bycatch** Sum of pelagic low bycatch and demersal non-destructive low bycatch pressures?  For CHI this is calculated with FAO fishing data in 1999 to 2003 and 2009 to 2011.  There are now 2012 data.  May be new SAUP data coming.  Will evaluate whether it is worth it to update these.

* **Pressure: hd_subtidal_sb:**  Ben B. had recommended not using "subtidal" because it was calculated using subtidal and soft shelf habitats (for HS we also included soft slope).  This is calculated at the region scale by summarizing the area of soft-bottom and relative proportion of destructive fishing for each eez/fao/etc and then applying the following [method](https://github.com/OHI-Science/ohiprep/blob/master/Global/HS_AQ_Pressures_HD_SB_2014/PressuresHD_subtidal_soft_bottom.png).  Questions: 1) should we apply formula at the raster scale? 2) what categories of soft-bottom do we want to include? 3) how do we incorporate new fishing data (will we have the different fishing types...or just assume equivalent increase/decrease per saup region)?

* **Pressure: po_chemicals_3nm:** Wait for John to update these data. Should be straight-forward.

* **Pressure: po_nutrients_3nm:** Wait for John to update these data.  Should be straight-forward.

* **Pressure: sp_alien:** This was based on the Molnar 2008 paper (data on the MEOW scale).  If we continue with this approach, I don't think there are any updated.  Would it be better to use John's alien data based on shipping models?  Also, I noticed that aquamaps included invasive species.  That might be another approach - but it would be fairly difficult.

* **Pressure: sp_genetic:** Trujillo data from 2008. No updates for genetic escapes, but updats on other data.  



###Done (or not doing)
* **Pressure: Artisanal high bycatch** Data from *Reefs at Risk Revisited* study - which hasn't been updated.

* **Organizing pressure reference points** Create a table that indicates the reference points used to rescale the pressures (https://docs.google.com/a/nceas.ucsb.edu/spreadsheets/d/1FMSqD4vBxYTBCsyejFnOdw-MfE_DgGetZ3B0CS8-fto/edit?usp=sharing).


###Future improvements
* **LSP:** Analyze in open source software.




##Following are Ben's notes

Global scenarios of the Ocean Health Index

The [calculate.R](./calculate.R) script generates the final global2014/scores.csv. The following sequence of calculating each scenario is needed:

1. **global2013**
1. **global2012** must follow global2013 in order to load global2013/scores.csv for LE Eritrea issue
