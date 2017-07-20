# Ocean Health Index British Columbia: /prep/tr/v2017

This folder describes the methods used to prepare data for Tourism and Recreation for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#tourism-and-recreation).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC data prep: Tourism and Recreation

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/tr/v2017/data_prep_tr.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/tr/v2017/data_prep_tr.html

### Summary:

OHIBC Tourism and Recreation

This script prepares layers (Visitors to parks and visitors centers) for Tourism and Recreation goal in 
British Columbia's coastal regions.  

We will analyze Livelihoods according to the model:

$x_{TR} = frac{P' + V'}{2}$

$P' = frac{P_c / P_{ref}}$

where P is total visitation to tourism centers found within each BC coastal region, compared to a reference period over the prior five years, and

$V' = frac{V_c / V_{ref}}$

where V is total visitation to parks found within each coastal region, compared to a reference period over the prior five years.


