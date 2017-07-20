# Ocean Health Index British Columbia: /prep/lsp/v2017

This folder describes the methods used to prepare data for Lasting Special Places for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#sense-of-place).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC goal prep: Lasting Special Places

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/lsp/v2017/data_prep_lsp.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/lsp/v2017/data_prep_lsp.html

### Summary:

OHIBC Lasting Special Places subgoal (Sense of Place)

The Lasting Special Places goal model is similar to the OHI Global model: a region's status is based upon percent of protected area within a certain amount inland and percent of protected area within 3 nautical mile offshore buffer, compared to a reference point of 30% protected area.  The major difference is that the inland zone for OHIBC will represent the coastal sub-watersheds, i.e. the sub-watersheds that intersect a 1 km inland buffer.

$$X_{LSP} = frac{frac{pA_{CMPA}}{pA_{refCMPA}} + frac{pA_{CP}}{pA_{refCP}}}{2}$$

*pA* = percent of area within the inland or offshore buffer; *CMPA* = coastal marine protected area (3nm offshore); *CP* = coastline protected (coastal watersheds inland); and *refCMPA* = *refCP* = 30% reference point for both measures.

Future changes may incorporate other data sets and MaPP planning zones.


