## Ocean Health Index British Columbia: /prep/spp_ico/v2017

This folder describes the methods used to prepare data for _GOALNAME_ for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#artisanal-fishing-opportunities).

The folders in this file include the metadata, R scripts, and data for each assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

### OHIBC data prep: Iconic Species:

* __Summary:__ OHIBC Iconic Species

This script prepares layers (species presence and species health) for Iconic Species subgoal in 
British Columbia's coastal regions.  Spatial data from IUCN and Aquamaps is
combined with extinction risk information from IUCN and conservation rank
info based on province-level NatureServe categories.

Iconic Species status is based upon a simple average of species
health for species found within each OHIBC region.

From Halpern et al (2012):

> The reference point is to have the risk status of all assessed species as Least Concern (i.e., a goal score = 1.0). Species that have not been assessed or labeled as data deficient are not included in the calculation.

**Mean risk status for OHIBC:**

$$bar{R} = frac{displaystylesum_{species}(Risk)}{n_{spp}}$$

**Iconic Species goal model**

$$X_{SPP} = (1 - bar{R}_{ICO}) * 100%$$

where:

* $X_{ICO}$ is Species goal status
* $bar{R}$ is mean extinction risk for identified species within OHIBC (different subsets for ICO and SPP)
* *Risk* is scaled value for species extinction risk category, based on: 
    * 'LC' = 0.0, 'NT' = 0.2, 'VU' = 0.4, 'EN' = 0.6, 'CR' = 0.8, 'EX' = 1.0
* ICO trend is calculated as the linear trend of the average extinction risk categories over time. * __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/spp_ico/v2017/data_prep_ico.Rmd * __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/spp_ico/v2017/data_prep_ico.html

### OHIBC goal prep: Species (Biodiversity subgoal):

* __Summary:__ OHIBC Species Subgoal (Biodiversity)

This script prepares scores (status and trend) for species richness in 
British Columbia's coastal regions.  Spatial data from IUCN and Aquamaps is
combined with extinction risk information from IUCN and conservation rank
info based on province-level NatureServe categories.

Currently, the Species Richness sub-goal model is identical to the OHI Global 
model: a region's status is based upon an area-weighted average of species
health across each BC reporting region.

From Halpern et al (2012):

> The target for the Species sub-goal is to have all species at a risk status of Least Concern. We scaled the lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions and would constitute a catastrophic loss of biodiversity. The Status of assessed species was calculated as the area- and threat status-weighted average of the number of threatened species within each 0.5 degree grid cell.

For BC, our calculation will be slightly different, though the end result will be identical.

**Species area-weighted risk:**  For each species within a region, the risk score is weighted by the proportion of the species' range within the given region.  To determine the mean area-weighted risk, the area-weighted risks are summed and divided by the total number of species within the region.

$$R_{spp/rgn} = (Risk)*frac{displaystylesum_{rgn}(A_{cell} * pA_{cell/rgn})}{A_{rgn}}$$

$$bar{R}_{spp} = frac{displaystylesum_{species}(R_{spp/rgn})}{n_{spp}}$$

**Species goal model**

$$X_{SPP} = frac{((1 - bar{R}_{spp}) - 0.25)}{(1 - 0.25)} * 100%$$

where:

* $X_{SPP}$ is Species goal status
* $R_{spp/rgn}$ is area-weighted extinction risk for one species within a region
* $bar{R}_{spp}$ is area-weighted mean extinction risk for a region
* $A_{cell}$ is cell area
* $pA_{cell-rgn}$ is percent of cell area included in region
* *Risk* is scaled value for species extinction risk category, based on: 
    * 'LC' = 0.0, 'NT' = 0.2, 'VU' = 0.4, 'EN' = 0.6, 'CR' = 0.8, 'EX' = 1.0
* SPP trend is calculated by examining the linear trend of mean extinction risk category, based upon the time series of risk categories from the IUCN Red List.  This calculation is performed in functions.R. * __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/spp_ico/v2017/data_prep_spp.Rmd * __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/spp_ico/v2017/data_prep_spp.html

### OHIBC: ICO custom species info:

* __Summary:__ Get species info, time series assessment, and spatial distribution for custom species for OHIBC ICO.

These custom species are considered iconic, yet are not considered marine, so their info is not readily available from the global SPP analysis.

``` {r get_custom_spp_info}

custom_spp_id <- 22679935

custom_info <- read_csv(file.path(dir_goal_anx_global, 'int', 'spp_info_from_api.csv')) %>%
  filter(iucn_sid %in% custom_spp_id)

```


``` {r setup_API_functions}

library(parallel)
library(jsonlite) * __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/spp_ico/v2017/ico_custom_spp_info.Rmd * __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/spp_ico/v2017/ico_custom_spp_info.html

-----

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!
