# Instructions for updating methods

## Running:
 
 # 4. Knit this file. 



1. Run the ohi_global/global_supplement/CombineLayers.R.  This creates an Rmd file called layers_all.Rmd.
2. Save a new OHI.bib file from Zotero if there are changes to referenes
3. Run BIBcorrect.R to fix a few weird things in the OHI.bib file
4. Knit the ohi-global/global_supplement/Supplement.Rmd file

If you need to update the descriptions in tables, do it in the .Rmd files located in global_supplement/layers_info.

If you need to update any of the other information (name, units, references, etc.), do this in the csv files located in ohi-global/eez_layers_meta_data.  NOTE: **DO NOT** change the "layer" name....this will mess everything up.

