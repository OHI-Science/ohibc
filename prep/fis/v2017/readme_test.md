# Ocean Health Index British Columbia: /prep/fis/v2017

<!--This folder describes the methods used to prepare data for _GOALNAME_ for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#artisanal-fishing-opportunities).

-->

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

Process RAM database for British Columbia fisheries stock status and harvest levels.  Each RAM stock will be filtered to the RAM stock ID and RAM area (to separate region-specific stocks), and then the stock status parameters B/Bmsy and F/Fmsy for that stock for each year reported.

Total catch data for each stock for each year will be used to determine final weightings.  Since the distribution of catch data is not differentiated within the reporting region, we will use SAUP and/or DFO spatial catch data to determine the distribution and allocate the total catch proportionally.

-----

## OHIBC: data prep for wild-capture fisheries: spatializing RAM stocks

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/2_data_prep_fis_ram_spatial.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/2_data_prep_fis_ram_spatial.html

### Summary:

This script collects spatial boundaries for RAM stocks, using stock IDs from the RAM prep script and the boundaries created by Chris Free: https://marine.rutgers.edu/~cfree/ram-legacy-stock-boundary-database/

From this, we generate for each stock a region-by-region proportional area to apportion RAM catch to OHIBC regions.

-----

## OHIBC: plot FIS scores by stock and region

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/3_plot_fis_scores.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/3_plot_fis_scores.html

### Summary:

This script takes the output from `functions.R` FIS model and plots individual stock scores by year and relative contribution to score.

-----

## OHIBC: data prep for wild-capture fisheries: DFO fisheries datasets

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/data_prep_fis_dfo.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/data_prep_fis_dfo.html

### Summary:

Processing BC DFO fisheries spatial data, to help determine allocation of total catch (from RAM catch values) to each OHIBC region.

-----

## data_prep_fis_ram_modeled.html

* __Rmd file:__ no Rmd present 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/data_prep_fis_ram_modeled.html

### Summary:

NA

-----

## data_prep_fis_saup.html

* __Rmd file:__ no Rmd present 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/data_prep_fis_saup.html

### Summary:

NA

-----

## OHIBC: data prep for wild-capture fisheries: Sea Around Us data

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/x_data_prep_fis_saup.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/x_data_prep_fis_saup.html

### Summary:

Process Sea Around Us data for British Columbia, to determine spatial distribution of catch for each species in BC.  The catch levels are used to weight the scores for fisheries stock status scores.  Stock status will be determined using RAM data for B/Bmsy and F/Fmsy.

This script will identify OHIBC-relevant stocks included in SAUP and extracting cell-by-cell values of catch for these fisheries.  Further spatialization (in `3_data_prep_fis_spatial.Rmd`) will allocate SAUP catch to RAM stocks and OHIBC regions.

Within this script we will also create a SAUP cell to OHIBC region lookup table with area-weighted allocations of cells to regions.

-----

## OHIBC: data explore for wild-capture fisheries: Sea Around Us data

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/x_plot_saup.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/fis/v2017/x_plot_saup.html

### Summary:

NA

-----

## OHIBC: data prep for wild-capture fisheries: spatializing RAM and SAUP stocks

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/fis/v2017/data_prep_fis_spatial_old.Rmd 
* __HTML file:__ no HTML present

### Summary:

This script collects spatial boundaries for RAM stocks, using stock IDs from the RAM prep script and the boundaries created by Chris Free: https://marine.rutgers.edu/~cfree/ram-legacy-stock-boundary-database/

The basic idea is to attribute each SAUP cell to an OHIBC region (or proportionally divide it across OHIBC regions) and to RAM stock regions. For each RAM-identified stock, we assign a SAUP surrogate stock by species or by closest taxon.

* First, SAUP catch data for the RAM stock is narrowed to just those cells that fall within the RAM stock region 
    * e.g. for West Coast Vancouver Island herring, we identify the SAUP cells that fall within the WCVanI region, and assign herring catches within those cells to that stock.
    * other SAUP herring cells are assigned to other RAM herring stocks accordingly.
* Once the stock-specific cells are identified, those cells are then assigned to OHIBC regions proportionally.

The final layer is a dataframe containing: year, OHIBC region, RAM area, SAUP info (taxonkey,sector,catch_type,reported), RAM stock ID, and then the total SAUP-reported catch for those parameters.


