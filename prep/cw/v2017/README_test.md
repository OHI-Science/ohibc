# Ocean Health Index British Columbia: /prep/cw/v2017

This folder describes the methods used to prepare data for Clean Waters for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#clean-waters).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC: CW chemical layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/cw/v2017/cw_chem_prep.rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/cw/v2017/cw_chem_prep.html

### Summary:

These data were prepared for the global 2016 assessment.  For each layer, we are simply reprojecting the data to the BC EEZ and rescaling to the local 99.99th percentile.

The Status of chemical pollution was measured via four global datasets: land-based organic from agricultural pesticide use (reported to FAO), inorganic pollution from runoff from impervious surfaces, and ocean-based pollution from commercial shipping and ports.

The shipping, ports and harbors, and inorganic pollution have not changed over time.  However, the organic (pesticide) pollution is updated yearly, with the most recent data from 2013.  The raw shipping, ports and harbors, and inorganic pollution data are located [here](https://knb.ecoinformatics.org/

-----

## OHIBC: Clean Waters goal prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/cw/v2017/cw_goal_est.rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/cw/v2017/cw_goal_est.html

### Summary:

This script pulls together data from the four components of Clean Waters and outputs layers for toolbox:

* chemical pollution, which includes land-based organic chemicals (pesticides), land-based inorganic chemicals (impervious surface runoff), and ocean-based pollution from shipping and harbors.
    * processed by `cw_chem_prep.Rmd`; log-transformed and normalized by 99.99%tile within EEZ.
    * as in the global analysis, this is assessed using the 3 nm coastal buffer zone.
    * details: 
        * years       : 2002 - 2013
        * resolution  : 1000, 1000  (x, y) in m
        * coord. ref. : `+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs` 
        * values      : 0, 2.889625  (min, max) (for first layer)
* nutrient pollution, which includes nutrient runoff from agriculture
    * processed by `cw_nutrient_prep.Rmd`; log-transformed and normalized by 99.99%tile within EEZ.
    * as in the global analysis, this is assessed at the 3 nm coastal buffer zone.
    * details: 
        * years       : 2002 - 2013
        * resolution  : 1000, 1000  (x, y) in m
        * coord. ref. : `+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs` 
        * values      : 0, 1  (min, max)
* marine debris, which includes marine plastics debris surface density by mass
    * processed by `cw_trash_prep.Rmd`; log-transformed and normalized by 99.99%tile within __NE Pacific__ rather than BC EEZ.
    * as in the global analysis, this is assessed across the entire EEZ.
    * details: 
        * years       : no time series
        * resolution  : 1000, 1000  (x, y) in m
        * coord. ref. : `+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs` 
        * values      : 0.2782703, 1  (min, max)

* pathogens, processed TBD.

This script pulls in finalized raster layers for each component, determines a score for each component by OHIBC region, then determines the overall score using a geometric mean calculation.

-----

## OHIBC: CW nutrient/fertilizer layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/cw/v2017/cw_nutrient_prep.rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/cw/v2017/cw_nutrient_prep.html

### Summary:

This data was used in the Clean Waters goal in OHI 2015

-----

## OHIBC: CW trash layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/cw/v2017/cw_trash_prep.rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/cw/v2017/cw_trash_prep.html

### Summary:

This data is incorporated into the OHI British Columbia Clean Waters (CW) goal.


