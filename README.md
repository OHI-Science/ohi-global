Preparing for 2015
============
Here is where we are (I have only appraised the goals and about half of the pressures so far).

Here is a [link](https://docs.google.com/a/nceas.ucsb.edu/spreadsheets/d/13rYgPYu9PgqsRTpkXikqVuDtdKZUN5Vs1yOsP5Do5ys/edit?usp=sharing) to a summary of each data layer so far.

###Working on now:
* **Summarizing the information**  Still need to get through about half of the pressures and all of the resilience measures.  I also need to look through issues and "new data" references.  As I go through the data, I will also record instances of gap-filling in this [issue](https://github.com/OHI-Science/issues/issues/351).

* **Organizing pressure reference points** Create a table that indicates the reference points used to rescale the pressures (https://docs.google.com/a/nceas.ucsb.edu/spreadsheets/d/1FMSqD4vBxYTBCsyejFnOdw-MfE_DgGetZ3B0CS8-fto/edit?usp=sharing).

* **LSP** Updates to these data.  Casey is running the python script [Issue 355](https://github.com/OHI-Science/issues/issues/355).  We are considering doing the data extraction using R.  I will need to get the script ready.

* **Pressure: Acid** Jamie is getting these spatial data together.  I have a question about whether there are biological data we could use to scale the data.

* **Pressures: General** Our pressure layers for 2008 and 2013 are typically aggregated data for several years (e.g., sst=2000-2005 and 2005-2010, etc).  So, is it worth updating these?  If so, what is the best approach? 

* **Pressure: UV** New data is [available](http://disc.sci.gsfc.nasa.gov/data-holdings/PIP/erythemal_uv_irradiance.shtml). Met with Jamie on Friday to discuss this goal.  We figured out how it was calculated in the past and Jamie is writing an issue to describe this.

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

* **Pressure: SST** Data is available for new year.  Might be worth extracting other years.  (check on emails Feb 9 2014)

* **Pressure: Commercial high bycatch** Is this the sum of pelagic high bycatch and demersal non-destructive high bycatch pressures?  In CHI this is calculated with FAO fishing data in 1999 to 2003 and 2009 to 2011.  There are now 2012 FAO data.  I suspect the 2008 data were used to calculate this pressure.  So we could easily update with 2013....is it worth trying to update further?

* **Pressure: Commercial low bycatch** Is this the sum of pelagic low bycatch and demersal non-destructive low bycatch pressures?  In CHI this is calculated with FAO fishing data in 1999 to 2003 and 2009 to 2011.  There are now 2012 FAO data.  I suspect the 2008 data were used to calculate this pressure.  So we could easily update with 2013....is it worth trying to update further?

* **Pressure: target harvest** It appears that the last analysis went to 2012, but I can't find the original data files to confirm this.  Maybe Katie knows where they are?  I will check with her.



###Things to keep checking on:
* **FIS** May have updated catch data.  May also have an updated CMSY model.

* **MAR** We have data to update coastal population to 2015 (Population data is projected from <=2000 data - but that may be the best available?).  There hasn't been an update yet for the FAO harvest tonnes (look for addition of 2013 [data](http://www.fao.org/fishery/statistics/global-aquaculture-production/en)).

* **AO** The new ppppcgdp data is not yet available.

* **FP** Update relative FIS to MAR when updates to these data.
* 
* * **CW** This will be updated with new pressures data for fertilizer and pesticide from John's plume models (see [issue](https://github.com/OHI-Science/issues/issues/343)), and the trend will be calculated directly from these pressures.  There is a new trash layer (that I haven't had a chance to look at - but this needs to be done). There is currently no new [pathogen](http://www.wssinfo.org/data-estimates/table) data available.


###Done (or not doing)
* **Pressure: Artisanal high bycatch** Data from *Reefs at Risk Revisited* study - which hasn't been updated.

* **Pressure: Artisanal low bycatch** "Modeled least destructive commercial fishing practices by 2 different gear types" (is this something we can update?)

###Extra stuff to do if there is time! (hahahahaha)





##Following are Ben's notes

Global scenarios of the Ocean Health Index

The [calculate.R](./calculate.R) script generates the final global2014/scores.csv. The following sequence of calculating each scenario is needed:

1. **global2013**
1. **global2012** must follow global2013 in order to load global2013/scores.csv for LE Eritrea issue
