# Ocean Health Index British Columbia: /prep/fis/v2017

This folder describes the methods used to prepare data for Wild-Capture Fisheries for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#food-provision).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC: data prep for wild-capture fisheries: RAM Database

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/1_data_prep_fis_ram.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/1_data_prep_fis_ram.html

### Summary:

Process RAM database for British Columbia fisheries stock status.  Each RAM stock will be filtered to the RAM stock ID and RAM area (to separate region-specific stocks), and then the stock status parameters B/Bmsy and F/Fmsy for that stock for each year reported.

Total catch data for each stock for each year will be used to determine final weightings.  We will use DFO data to weight by catch.

-----

## OHIBC: data prep for wild-capture fisheries: DFO catch data

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/2_data_prep_fis_dfo.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/2_data_prep_fis_dfo.html

### Summary:

This script preps the DFO catch data for use in the BC Fisheries model.

-----

## Calculating FIS, MAR, and SAL weights from DFOs Year in Review reports

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/3a_data_prep_fis_mar_sal_weighting.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/3a_data_prep_fis_mar_sal_weighting.html

### Summary:

NA

-----

## OHIBC: plot FIS scores by stock and region

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/4_plot_fis_scores.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/4_plot_fis_scores.html

### Summary:

This script takes the output from `functions.R` FIS model and plots individual stock scores by year and relative contribution to score.


