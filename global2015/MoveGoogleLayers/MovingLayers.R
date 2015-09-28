#################################################
## Cleaning and moving Google Layers documents
## so they are in ohi-global
#################################################

#### eez ----

# Step 1: download the entire document and preserve.  This was the document used for the OHI2015 analysis.
## also saved as archive Google doc file here: https://docs.google.com/spreadsheets/d/1JtFuDNuW5bt8RGPNnmWtZd3ZGiIQXuaJ8Kq9K3eBfGw/edit#gid=0)
# load Google spreadsheet for copying layers

key <- "1qGfo99QXyTFWSm__8l_NwV-AYNmSoxjignFcm8Z0Zas"
g.url = sprintf('https://docs.google.com/spreadsheets/d/%s/export?gid=0&format=csv', key)
g = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='', stringsAsFactors=F)

## can use this to recreate if I accidentally delete any variables that are important:
#write.csv(g, 'global2015/MoveGoogleLayers/archive/layers_2015_allVariables.csv', na='', row.names=F)


# Step 2: Download the newest version with the unused variables deleted.
# This will be our working copy
write.csv(g, 'layers_eez.csv', na='', row.names=F)


#### high seas ----

# Step 1: download the entire document and preserve.  This was the document used for the OHI2015 analysis.
## also saved as archive Google doc file here: https://docs.google.com/spreadsheets/d/1JtFuDNuW5bt8RGPNnmWtZd3ZGiIQXuaJ8Kq9K3eBfGw/edit#gid=0)
# load Google spreadsheet for copying layers

key <- "1qGfo99QXyTFWSm__8l_NwV-AYNmSoxjignFcm8Z0Zas"
g.url = sprintf('https://docs.google.com/spreadsheets/d/%s/export?gid=0&format=csv', key)
g = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='', stringsAsFactors=F)

## can use this to recreate if I accidentally delete any variables that are important:
#write.csv(g, 'global2015/MoveGoogleLayers/archive/layers_2015_allVariables.csv', na='', row.names=F)


# Step 2: Download the newest version with the unused variables deleted.
# This will be our working copy
write.csv(g, 'layers_eez.csv', na='', row.names=F)

