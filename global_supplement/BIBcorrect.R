setwd("global_supplement")

## NOTE: Have to hand correct the US Deaprtment of State for some reason

bib <- readLines("OHI.bib")
bib <- gsub(pattern = "Birdlife International", "{{Birdlife International}}", bib)
bib <- gsub(pattern = "United Nations personal communication", "{{United Nations personal communication}}", bib)
bib <- gsub(pattern = "US Department of State", "{{US Department of State}}", bib)
bib <- gsub(pattern = "World Bank", "{{World Bank}}", bib)
bib <- gsub(pattern = "World Economic Forum", "{{World Economic Forum}}", bib)
bib <- gsub(pattern = "Center for International Earth Science Information Network (CIESIN)", 
             "{{Center for International Earth Science Information Network}}", bib)
bib <- gsub(pattern = "Columbia University", 
             "{{Columbia University}}", bib)
bib <- gsub(pattern = "United Nations Food and Agriculture Programme (FAO)", 
             "{{United Nations Food and Agriculture Programme}}", bib)
bib <- gsub(pattern = "Centro Internacional de Agricultura Tropical (CIAT)", 
             "{{Centro Internacional de Agricultura Tropical}}", bib)
bib <- gsub(pattern = "Convention on Biological Diversity", "{{Convention on Biological Diversity}}", bib)
bib <- gsub(pattern = "Eionet, European Environment Agency (EEA)", "{{Eionet European Environment Agency}}", bib)
bib <- gsub(pattern = "European Commission", "{{European Commission}}", bib)
bib <- gsub(pattern = "Joint Research Centre", "{{Joint Research Centre}}", bib)
bib <- gsub(pattern = "Organisation for Economic Co-operation and Development", 
            "{{Organisation for Economic Co-operation and Development}}", bib)
bib <- gsub(pattern = "Joint Nature Conservation Committee", "{{Joint Nature Conservation Committee}}", bib)
bib <- gsub(pattern = "Millennium Ecosystem Assessment", "{{Millennium Ecosystem Assessment}}", bib)
bib <- gsub(pattern = "New Zealand Ministry for the Environment", "{{New Zealand Ministry for the Environment}}", bib)
bib <- gsub(pattern = "Ocean Conservancy", "{{Ocean Conservancy}}", bib)
bib <- gsub(pattern = "R Core Team", "{{R Core Team}}", bib)
bib <- gsub(pattern = "Secretariat of the Convention on Biological Diversity", 
            "{{Secretariat of the Convention on Biological Diversity}}", bib)
bib <- gsub(pattern = "United Nations", "{{United Nations}}", bib)
bib <- gsub(pattern = "US Department of Commerce", "{{US Department of Commerce}}", bib)
bib <- gsub(pattern = "NOAA National Centers for Environmental Information", 
            "{{NOAA National Centers for Environmental Information}}", bib)
bib <- gsub(pattern = "EDF France", "{{EDF France}}", bib)
bib <- gsub(pattern = "Joint Nature Conservation Committee (JNCC)", "{{Joint Nature Conservation Committee}}", bib)
bib <- gsub(pattern = "{US} Department of State", "{{US Department of State}}", bib)
bib <- gsub(pattern = "Government of Canada", "{{Government of Canada}}", bib)


writeLines(bib, "OHI.bib")
