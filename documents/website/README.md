# Instructions for updating layers data

1. Run the ohi_global/eez/conf/CombineLayers.R.  This creates an Rmd file called layers_all.Rmd.
2. Open the layers_all.Rmd and knit.
3. Check to see that everything looks ok.

If you need to update the description, do it in the .Rmd files located in global_supplement/layers_info.

If you need to update any of the other information (name, units, references, etc.), do this in the csv files located in ohi-global/eez_layers_meta_data.  NOTE: **DO NOT** change the "layer"" name....this will mess everything up.

