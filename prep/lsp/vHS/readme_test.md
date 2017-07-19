# Ocean Health Index British Columbia: /prep/lsp/vHS

<!--This folder describes the methods used to prepare data for _GOALNAME_ for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#artisanal-fishing-opportunities).

-->

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

The directories in this goal directory include metadata, R/Rmd scripts, intermediate data processing, and layer outputs for each assessement year (i.e., the year the assessment was conducted, for OHIBC currently only 2017) or scenario (e.g. Howe Sound OHIBC assessment).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC Howe Sound: Lasting Special Places

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/lsp/vHS/goal_prep_lsp.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/lsp/vHS/goal_prep_lsp.html

### Summary:

OHIBC Lasting Special Places subgoal (Sense of Place)

The Lasting Special Places goal model for Howe Sound is similar to the OHI Global model: a region's status is based upon percent of protected area within 1 km inland buffer and percent of protected area within 3 nautical mile offshore buffer, compared to a reference point of 30% protected area.

$$X_{LSP} = frac{frac{pA_{CMPA}}{pA_{refCMPA}} + frac{pA_{CP}}{pA_{refCP}}}{2}$$

*pA* = percent of area within the inland or offshore buffer; *CMPA* = coastal marine protected area (3nm offshore); *CP* = coastline protected (1km inland); and *refCMPA* = *refCP* = 30% reference point for both measures.

For Howe Sound, based upon conversations with Andrew Day and Karin Bodtker, the weighting between onshore and offshore protected area will be area-weighted rather than a simple 50/50 weighting as in the OHI Global.

An alternative will also examine protected areas within the entire watershed that feeds Howe Sound, rather than simply a 1 km inland buffer.
