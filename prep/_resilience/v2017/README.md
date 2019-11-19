# Ocean Health Index British Columbia: /prep/_resilience/v2017

This folder describes the methods used to prepare data for Resilience for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC: Resilience - Community Well Being Index prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_resilience/v2017/data_prep_cwb.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_resilience/v2017/data_prep_cwb.html

### Summary:

This pressure layer calculates the social resilience of communities based on the Community Well-Being Index.  Here we calculate resilience scores for each of the four components (income, education, housing, and labor force activity) as well as the composite CWB.  We calculate this for overall population (non-indigenous and First Nations combined) and First Nations, using an area-weighted mean of scores by census district. 

$$resilience_{rgn} = frac{sum_{i=1}^N (CWB_{csd} * pop_{csd})}{sum_{i=1}^N pop_{csd}}$$

where $CWB_{csd}$ is the CWB score (component or total) for each of $N$ census subdivisions within the OHIBC region, and pop_{csd} is the portion of the population of that CSD within the OHIBC region.

-----

## OHIBC: Resilience - Food provision biomass removal resilience prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_resilience/v2017/data_prep_fp_biomass_resilience.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_resilience/v2017/data_prep_fp_biomass_resilience.html

### Summary:

This script prepares a resilience layer based upon the presence, effectiveness, and compliance with fishing management regulations.  As in several other resilience layers, it is a geometric mean of three components: 

$$resilience = sqrt[3]{reg.presence times reg.effectiveness times reg.compliance}$$

UPDATE: now we simply give credit for 1/3, 1/3, 1/3 for each of these components.  If a component is missing, no credit is due.  If a component is not binary, its 1/3 will be scaled according to a proportionality metric.

$$resilience = frac{reg.presence + reg.effectiveness + reg.compliance}{3}$$

-----

## OHIBC: Resilience - MaPP social resilience

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_resilience/v2017/data_prep_mapp_resilience.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_resilience/v2017/data_prep_mapp_resilience.html

### Summary:

This layer calculates social resilience based on the MaPP process.  

$$resilience_{rgn} = mathbb{1}_{rgn in MaPP} times frac{process + enforcement + compliance}{3}$$

From the start of the MaPP process, each region within MaPP will receive a resilience score of 1/3.  Enforcement and compliance would begin to contribute to the score once the MaPP plans are released in 2015, but since only two of the three parties (First Nations, province, but not DFO) each region will receive only partial credit.


