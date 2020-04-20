# Ocean Health Index British Columbia: /prep/lv/v2017

This folder describes the methods used to prepare data for Livelihoods and Economies for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#livelihoods-and-economies).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC data prep: Livelihoods

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/lv/v2017/data_prep_lv.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/lv/v2017/data_prep_lv.html

### Summary:

OHIBC Livelihoods

This script prepares layers (employment rates and median income) for Livelihoods goal in 
British Columbia's coastal regions.  

From Halpern et al. (2014) (OHI California Current):

>Livelihood sub-goal: As was done in the global analysis, coastal livelihoods is measured by two equally weighted sub-components, the number of jobs (j), which is a proxy for livelihood quantity, and the median annual household wages (g), which is a proxy for job quality. For jobs and wages we used a no-net loss reference point. 

For British Columbia, we do not currently have sector-specific unemployment and wage information.  As such we will analyze Livelihoods according to the model:

$x_{LIV} = frac{j' + g'}{2}$

$j' = frac{j_c / j_{ref}}{M_c / M_{ref}}$

where M is each region’s employment rate (1 - unemployment) as a percent at current (c) and reference (ref) time periods, and:

$g' = frac{g_c / g_{ref}}{W_c / W_{ref}}$

where W is each region’s average annual per capita wage at current (c) and reference (ref) time periods.


