# Ocean Health Index British Columbia: /prep/spp_ico/vHS

This folder describes the methods used to prepare data for Species and Iconic Species for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC Howe Sound goal prep: Iconic Species

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/spp_ico/vHS/goal_prep_ico_howesound.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/spp_ico/vHS/goal_prep_ico_howesound.html

### Summary:

OHIBC Howe Sound Species and Iconic Species

This script prepares scores (status and trend) for species richness in 
British Columbia's Howe Sound region.  Spatial data from IUCN and Aquamaps is
combined with extinction risk information from IUCN and conservation rank
info based on province-level NatureServe categories.

Because of the small scale of Howe Sound, neither Species nor Iconic Species goals use any area weighting.  Instead, ICO and SPP status is based upon a simple average of species
health for species found within the Howe Sound region.

From Halpern et al (2012):

> The target for the Species sub-goal is to have all species at a risk status of Least Concern. We scaled the lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions and would constitute a catastrophic loss of biodiversity. The Status of assessed species was calculated as the area- and threat status-weighted average of the number of threatened species within each 0.5 degree grid cell.

**Mean risk status for Howe Sound:**

$$bar{R} = frac{displaystylesum_{species}(Risk)}{n_{spp}}$$

**Iconic Species goal model**

$$X_{SPP} = 1 - bar{R}_{ICO} * 100%$$

where:

* $X_{ICO}$ is Species goal status
* $bar{R}$ is mean extinction risk for identified species within Howe Sound (different subsets for ICO and SPP)
* *Risk* is scaled value for species extinction risk category, based on: 
    * 'LC' = 0.0, 'NT' = 0.2, 'VU' = 0.4, 'EN' = 0.6, 'CR' = 0.8, 'EX' = 1.0
* ICO trend is calculated as the linear trend of the average extinction risk categories over time.

-----

## OHIBC Howe Sound goal prep: Species

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/spp_ico/vHS/goal_prep_spp_howesound.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/spp_ico/vHS/goal_prep_spp_howesound.html

### Summary:

OHIBC Howe Sound Species

This script prepares scores (status and trend) for species richness in 
British Columbia's Howe Sound region.  Spatial data from IUCN and Aquamaps is
combined with extinction risk information from IUCN and conservation rank
info based on province-level NatureServe categories.

Because of the small scale of Howe Sound, neither Species nor Iconic Species goals use any area weighting.  Instead, ICO and SPP status is based upon a simple average of species
health for species found within the Howe Sound region.

From Halpern et al (2012):

> The target for the Species sub-goal is to have all species at a risk status of Least Concern. We scaled the lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions and would constitute a catastrophic loss of biodiversity. The Status of assessed species was calculated as the area- and threat status-weighted average of the number of threatened species within each 0.5 degree grid cell.

**Mean risk status for Howe Sound:**

$$bar{R} = frac{displaystylesum_{species}(Risk)}{n_{spp}}$$

**Species goal model**

$$X_{SPP} = frac{((1 - bar{R}_{SPP}) - 0.25)}{(1 - 0.25)} * 100%$$

where:

* $X_{SPP}$ is Species goal status
* $bar{R}$ is mean extinction risk for identified species within Howe Sound (different subsets for ICO and SPP)
* *Risk* is scaled value for species extinction risk category, based on: 
    * 'LC' = 0.0, 'NT' = 0.2, 'VU' = 0.4, 'EN' = 0.6, 'CR' = 0.8, 'EX' = 1.0
* SPP trend is calculated as the linear trend of the average extinction risk categories over time.


