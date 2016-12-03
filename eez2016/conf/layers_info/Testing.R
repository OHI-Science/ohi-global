

tmp <- capture.output(cat("---", 
                          "\ntitle: Layers descriptions",
                          "\noutput:",
                          "\n  html_document:",
                          "\n    toc: true",
                          "\n    toc_depth: 1",
                          "\n    number_sections: true",
                          "\n---"))

write(tmp, "layers_all.Rmd")


data <- data.frame(titles = c("Chemicals", "Nutrients"),
                   files = c("po_chemicals.Rmd", "po_nutrients.Rmd"))

for(title in data$titles){ #title="Chemicals"

file <- data$files[data$titles == title]

tmp <- capture.output( cat("\n",  
                          paste0("\n#", title),
                          "\n",
                          paste0("\n```{r,",sprintf(" child = '%s'", file), ", echo=FALSE}"),
                          "\n",
                          "\n```",
                          "\n",
                          "\n###References {-}"))

write(tmp, "layers_all.Rmd", append=TRUE)
}      