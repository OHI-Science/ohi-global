# Instructions for updating methods

## Running:

1. Run the ohi_global/documents/methods/CombineLayers.R.  This creates an Rmd file called layers_all.Rmd.
2. Save a new OHI.bib file from Zotero if there are changes to references
3. Knit the ohi-global/documents/methods/Supplement.Rmd file

If you need to update the descriptions in tables, do it in the .Rmd files located in global_supplement/layers_info.

If you need to update any of the other information (name, units, references, etc.), do this in the csv files located in ohi-global/metadata_documentation.  NOTE: **DO NOT** change the "layer" name....this will mess everything up.


# Document description for supplementary methods

To maximize flexibility, we created an individual Rmd files for each data layer and goal that describes the general methods used to obtain the data.  Ideally, these Rmd files do not include information that will change each year (e.g., final year of data used in analysis, links to data preparation files on Github, etc), so they can be used each year with minimal changes.  Components that change every year should be included in tables, such as those in _ohi-global/metadata_documentation.  R scripts are used to merge all the Rmd files, as well as any additional data from other locations.

**Description:**
*layers*
The Rmd layer descriptions are located in: ohi-global/metatdata_documentation/ohi_model/layers_info_

An example script that combines these into a single Rmd file (used for supplementary methods of OHI 2016) is: ohi-global/documents/methods/CombineLayers.R_

This creates a file called _layers_all.Rmd_

The _layers_all.Rmd_ file is then incorporated into the following document: _ohi-global/documents/methods/Supplement_Results.Rmd_

*goals*
Long goal descriptions are located here: _ohi-global/metadata_documentation/ohi_model/goal_descriptions_

These are merged together in: _ohi-global/metadata_documentation/ohi_model//Supplement.Rmd_
