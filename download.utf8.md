---
title: "Download global data and methods"
output: 
  html_document:
     includes: 
      in_header: 'ohi_hdr.html'
---


<!---From http://stackoverflow.com/questions/31753897/2-column-section-in-r-markdown:
Put in your css file or directly in rmarkdown--->
<style>
  .col2 {
    columns: 2 200px;         /* number of columns and width in pixels*/
    -webkit-columns: 2 200px; /* chrome, safari */
    -moz-columns: 2 200px;    /* firefox */
  }
</style>

<br>
Global OHI assessments require synthesizing highly heterogeneous information that is freely available from nearly one hundred sources. Data from each source are prepared and modeled using freely available coding and version control software. You may freely download and use any data, code, or instructional guides, but please see our [Citation Policy](http://ohi-science.org/citation-policy/).  

With each annual global assessment we have incrementally improved data processing and modeling methods based on new information, knowledge, and feedback. Thus, for annual scores to be comparable with previous years, we recalculate previous assessments with the most recent methods for each past assessment year. All code and most data are available online on GitHub.  

-----

<div class="col2">
**Global assessment scores 2012-2019**

Quick access to *.csv* files of calculated global scores.

- [Download by clicking here](https://github.com/OHI-Science/ohi-global/raw/published/yearly_results/global2019/OHI_final_formatted_scores_2019-11-15.csv), then right-click to Save As.
- [Description of scores data](https://github.com/OHI-Science/ohi-global/blob/draft/yearly_results/README.md#global-ohi-score-metadata).

**Documentation and Instruction**

- [2019 Global OHI Scores Report](http://htmlpreview.github.io/?https://github.com/OHI-Science/ohi-global/blob/published/yearly_results/global2019/Results/Supplement_Results.html)
- [2019 Methods](http://htmlpreview.github.io/?https://github.com/OHI-Science/ohi-global/published/documents/methods/Supplement.html)
- [Peer-reviewed publications](http://ohi-science.org/resources/publications/) 
- [Learn how to use `ohicore`](http://ohi-science.org/ohicore)

<br>

**Full methods and data**

Final scores and methods are released annually each fall. Ongoing work is available in our [**OHI-Science**](https://github.com/OHI-Science) GitHub repositories.
A description of file organization [is available](https://github.com/OHI-Science/ohiprep_v2018/blob/gh-pages/src/dataOrganization_SOP.md#sop-for-data-management-for-ocean-health-index-assessments).

- [data preparation releases (`ohiprep` repository)](https://github.com/OHI-Science/ohiprep_v2019) 
- [score calculation releases (`ohi-global` repository)](https://github.com/OHI-Science/ohi-global/releases)
- [`R` package to calculate scores (`ohicore` repository)](https://github.com/OHI-Science/ohicore/releases)
- [Large data files](https://mazu.nceas.ucsb.edu/data/): Download data too large to host on GitHub (region shapefiles, pressure raster data, etc.) 
</div>

<br>




---- 

## Acknowledgements

We thank all the groups that collect and share data openly; please see the [global layers page](http://ohi-science.org/ohi-global/layers_table.html) for a full list. We also thank developers and communities that make and teach free software tools, in particular [R](https://cran.r-project.org/), [RStudio](https://www.rstudio.com), [git](https://git-scm.com/), [GitHub](https://github.com/), and [rOpenSci](https://ropensci.org/).

[<img src="https://docs.google.com/drawings/d/1633uvIYgsBhiKBddQAaTlvYDJgOK1uiDB6wkooNyhFc/pub?w=576&h=288" width="100px">](http://ohi-science.org)  
[ohi-science.org home](http://ohi-science.org)

