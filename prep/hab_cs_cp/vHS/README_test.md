# Ocean Health Index British Columbia: /prep/hab_cs_cp/vHS

This folder describes the methods used to prepare data for Habitats, Carbon Storage, Coastal Protection for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC Howe Sound: Habitat goal (CP, CS, HAB) layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/hab_cs_cp/vHS/data_prep_hab_cp_cs_hs.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/hab_cs_cp/vHS/data_prep_hab_cp_cs_hs.html

### Summary:

Clip existing habitat layers (salt marsh/forest raster, salt marsh and seagrass polygons) for HAB, CP, CS goals for Howe Sound assessment.  For CP, calculate values based on exposure, elevation, and protective potential weighting as required for different protective habitats.  For CS, calculate values based on carbon storage potential.

questions to be answered: 

* CP coastal forests: buffer, elevation, etc? 
* CS coastal forests: full watershed or buffer?

-----

## OHIBC Howe Sound: sponge and soft-bottom data prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/hab_cs_cp/vHS/data_prep_hab_sponge_sb_hs.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/hab_cs_cp/vHS/data_prep_hab_sponge_sb_hs.html

### Summary:

create sponge and soft-bottom layers for HAB goals for Howe Sound specifically.  Sponge layers will be at a finer resolution than the overall BC layer, in order to better capture fine-scale sponge reef habitats. From these layers, calculate trawl pressures for each habitat type. We also calculate the area of sponge reefs protected by fishing exclusion areas as an alternate method.

-----

## OHIBC Howe Sound: Carbon Storage and Coastal Protection goals

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/hab_cs_cp/vHS/goal_prep_cp_cs_hs.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/hab_cs_cp/vHS/goal_prep_cp_cs_hs.html

### Summary:

Pull data layers to calculate Howe Sound goals: Coastal Protection, Carbon Storage, and Habitats

-----

## OHIBC Howe Sound: Habitat goal score

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/hab_cs_cp/vHS/goal_prep_hab_hs.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/hab_cs_cp/vHS/goal_prep_hab_hs.html

### Summary:

Pull data layers to calculate Howe Sound Habitats goal

- Seagrass
    - excluded from analysis: nutrient pressure insignificant; no model yet for log booms/development/sedimentation pressure
- Saltmarsh
    - land use change
- Coastal forests
    - excluded from analysis: does not directly support marine biodiversity
- Soft bottom
    - trawl pressure
- EBSAs? 
    - excluded from analysis: none present in Howe Sound
- Howe Sound-specific sponges
    - protection by closures, reference point 100% protected
    - alternative model based on trawl pressure


