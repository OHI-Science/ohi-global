---
layout: article
title: "Regions"
excerpt: "OHI regions for Baltic"
share: false
ads: false
branch_scenario: published/baltic2015
toc: true
---

Regions are the fundamental spatial units of analysis for the Ocean Health Index. Scores are calculated for each region individually and then combined (with an offshore area-weighted average) to produce scores for the entire study area, {{ site.study_area }}. These template regions can be modified with some effort. They were identified as the largest subcountry division within {{ site.study_area }}.

<nav class="navbar navbar-default" role="navigation">   <div class="container-fluid">     <div class="navbar-header">       <a class="navbar-brand" href="#">Branch/Scenario</a>     </div>     <div class="collapse navbar-collapse" id="navbar-1">       <ul class="nav navbar-nav">         <li class="dropdown">           <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">published/baltic2015<span class="caret"></span></a>           <ul class="dropdown-menu" role="menu">                       <li><a href="{{ site.baseurl }}/draft/baltic2015/regions/">draft/baltic2015</a></li>                     </ul>         </li>       </ul>     </div>   </div> </nav> 

![]({{ site.baseurl }}/results/{{ page.branch_scenario }}/figures/regions_600x400.png)

Or see the [interactive map of the offshore regions on Github](https://github.com/OHI-Science/{{ site.git_repo }}/blob/{{ page.branch_scenario }}/spatial/regions_gcs.geojson).

OHI {{ site.study_area }} has the following subcountry regions, each with a unique identifier (ID):

{% capture regions_csv %}regions_{{ page.branch_scenario | replace:'/','_' }}{% endcapture %}
{% assign regions = site.data[regions_csv] %}

| ID               | NAME            |
|-----------------:|:----------------|
{% for rgn in regions %}| {{ rgn.region_id }} | {{ rgn.rgn_title }} |
{% endfor %}

IDs for subcountry regions were assigned geographically by increasing longitude. The entire study area ({{ site.study_area }}) has a special region ID of 0.  

Exclusive economic zones (EEZs) were identified by [MarineRegions.org](http://www.marineregions.org) and the largest subcountry regions were identified by the generalized administrative boundary database [GADM.org](http://www.gadm.org). Region boundaries were extended offshore to divide the EEZ of {{ site.study_area }} into offshore regions. It is possible to use different regions than the ones provided here. See [Create Regions](http://ohi-science.org/pages/create_regions.html) for more details.

Regions were generated with the following inland and offshore buffers relative to the shoreline, which are visualized in the graphic above within increasing transparency away from the shoreline.

- offshore
- offshore1km
- offshore3nm
- inland
- inland1km
- inland25km

These data are available as shapefiles as well as csv files summarizing total area per region. Since shapefiles are binary and can be large, they're best stored outside the **{{ site.git_repo }}** Github repository. Instead, download the zipped set of shapefiles:

- [{{ site.git_repo }}_shapefiles.zip](http://ohi.nceas.ucsb.edu/data/subcountry2014/{{ site.git_repo }}_shapefiles.zip)

