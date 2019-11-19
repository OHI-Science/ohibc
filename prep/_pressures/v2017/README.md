# Ocean Health Index British Columbia: /prep/_pressures/v2017

This folder describes the methods used to prepare data for Pressures for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## Comparing Watson and SAUP data

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/fish_compare_saup_watson.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/fish_compare_saup_watson.html

### Summary:

NA

-----

## OHIBC: Alien Species Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_alien_spp.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_alien_spp.html

### Summary:

This pressure layer evaluates invasive species pressure on BC regions using the same method as OHI Global and California Current.  From Molnar (2008) data, we have data on the number of invasive species (and number of harmful invasives) found within each Marine Ecoregion of the World (MEOW) region.  For the global assessment and California Current, score is determined based on the proportion of invasives relative to the global maximum.  For OHIBC, we will instead use a species-by-species pressure based on three criteria: _invasive potential_, _ecological impact_, and _management difficulty_.  In Molnar 2008, each of these (as well as _scope_) is ranked on a 1 to 4 scale, with 1 being minimal and 4 being intense.  Invasive pressure of a species is a geometric mean (rescaled to max = 1) of these threat criteria:

$$pressure_{spp} = P_{spp} = frac{prod_{T=1}^N(threat_T)^{1/N}}{4}$$
where T is a threat criterion, N is number of threat criteria scores for that species (excluding NAs).

Scores for each OHIBC region are the area-weighted total species pressure from each ecoregion present.  This assumes that invasive species are uniformly distributed throughout the ecoregion.

$$X_{invasive} = frac{sum_{ecorgn=1}^{N}((sum_{spp=1}^M P_{spp})  * A_{ecorgn})}{sum_{ecorgn=1}^N A_{ecorgn}}$$
where $N$ is the number of ecoregions falling within an OHIBC region; $M$ is the number of species found within that ecoregion; $P_{spp}$ is the pressure score for each species in the ecoregion; and $A_{rgn}$ is the area of the OHIBC region allocated to that ecoregion (so $sum_{rgn=1}^NA_{ecorgn}$ becomes the total OHIBC region area).

The data has no time scale.

-----

## OHIBC: Commercial Fishing Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_fishing_prep_old.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_fishing_prep_old.html

### Summary:

The commercial fishing pressure layers are created from spatialized catch by gear data provided by Watson (2017), and net primary production data from the Vertically Generalized Production Model [(VGPM)](http://www.science.oregonstate.edu/ocean.productivity/) as described in [Behrenfeld and Falkowski (1997)](http://www.science.oregonstate.edu/ocean.productivity/references/L&O%201997a.pdf).

Two layers are created here, commercial fishing pressure from **high bycatch** gear and **low bycatch** gear. The raw spatial catch data provides information about species catch per cell along with information on the type of gear used. Each gear type was categorized into high and low bycatch gears based on the global Cumulative Human Impacts study (Halpern et al. 2008) and was done using [this script](https://github.com/OHI-Science/impact_acceleration/blob/master/stressors/comm_fish/watson_gear_matching.Rmd).

The layers created here are derived from the layers used for the global OHI assessment. The main difference, aside from only using data within the BC region, is the decision to not log transform the values. When using the full global extent of the data in the global assessment, we log transform to account for skew. Since we are using this data at a much smaller scale, we have decided not to log transform.

-----

## OHIBC: Commercial Fishing Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_fishing_prep.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_fishing_prep.html

### Summary:

The commercial fishing pressure layers are created from spatialized catch by gear data provided by Watson (2017).

Two layers are created here:

1. __Landings__ - representing the pressure that targeted catch exerts on the system
2. __Discards__ - representing the pressure that non-targeted catch exerts on the system

The reference point used for each is 110% of the maximum catch rate (tons/km2) within each sub region found across the entire time series (1950 - 2014).

-----

## OHIBC: Genetic Escapes Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_gen_escapes.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_gen_escapes.html

### Summary:

This pressure layer calculates the pressure due to genetic escapes from aquaculture.  The Mariculture Sustatinability Index ranks aquaculture species in different regions with a score from 1-10 for potential for genetic pollution.  For OHIBC we will follow a similar method as OHI Global and CC, rescaling genetic escape pressure from 0-1 for each species and calculating a harvest-weighted average of genetic escape pressure for each region.

$$pressure_{gen.escapes} = frac{sum_{spp=1}^N (MSI_{spp} * harvest_{spp})}{sum_{spp=1}^N harvest_{spp}}$$

The MSI scores are not time-dependent, but harvest data is reported annually.

-----

## OHIBC: Habitat Destruction Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_hd_intertidal_prep.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_hd_intertidal_prep.html

### Summary:

This script calculates potential for intertidal habitat destruction from trampling.  As a proxy for trampling damage we use the population density (based on census subdivisions) of the coastal area within 10 km of the shore.

$$pressure_{hd.intertidal} = frac{Pop_{10km}}{Area_{10km}}$$

Much of this analysis is similar to the calculations for population densities calculated in the Clean Waters/Pathogens component.  However, since the regional boundaries are different (10 km buffer rather than entire inland region) we will simply recalculate.

As an added bonus, it also copies over the HAB Soft Bottom Trawl layer over as a soft-bottom trawl habitat destruction layer.

-----

## OHIBC: Pressures for logging and mining

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_logging_prep.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_logging_prep.html

### Summary:

Generates pressures layers for logging activity.

Also generates a layer for mining activity which will be set to zeros for all observations, but preserves a placeholder for future mining pressures.

-----

## OHIBC: Ocean Acidification Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_oa_prep.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_oa_prep.html

### Summary:

This layer will determine ocean acidification values from the global rate of change layers; these layers already scale OA pressures appropriately.  The rescaling is based on the changes in aragonite saturation state according to:

$$Delta Omega_{year} = frac{(Omega_{base} - Omega_{year})}{(Omega_{base} - 1)}$$
    
In the global analysis, pressures for 2011-2016 are available with this rescaling; however, for this OHIBC analysis, we will begin with the global rasters for annual mean aragonite saturation state values, crop to BC, interpolate using inverse distance weighting, and then reproject to approx 1 km resolution (from CHI).

-----

## OHIBC: Sea Level Rise Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_slr_prep.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_slr_prep.html

### Summary:

There are two parts to creating this layer:
  
  1. Data prep to get raw data into the correct format:
  
    * If necessary, read .nc.gz files from aviso: ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/climatology/monthly_mean/
    * unzip .gz files and then delete .gz 
    * Read monthly mean sea level data as rasters

  2. Creating the pressure layers for OHI Global:  
  
    * Clip all monthly rasters to the coast using a 3 nautical mile offshore buffer
    * Calculate annual mean sea level anomaly rasters from monthly data
    * Determine a reference point as the 99.99th quantile of the data across all years (1993 - 2015)
    * Rescale values from 0 to 1 using the reference point
    * Set to zero all negative values, indicating decreases in mean sea level
    * Resample raster to ~ 1km2 and reproject to Molleweide

This process is completed within the Global OHI 2017 prep script.  

For OHIBC, we modify the OHI 2017 script to process the data within the BC region, in BC Albers projection, using a 1000 meter raster of the 3 nautical mile buffer.

-----

## OHIBC: Sea Surface Temperature Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_sst_prep.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_sst_prep.html

### Summary:

For OHI Global Assessments, we calculated a climatology from 1982-2012 per cell using weekly data from CorTAD. We then calculated the number of times a given cell's weekly SST was greater than the climatological mean for that week (an anomalous week: mean temp greater than historical mean + one standard deviation) and summed the number of anomalous weeks in a single year. The maximum value a cell could have is 52 which would mean that cell had anomalous SST temperatures for each week of the year.

To account for annual variation, we look at Sea Surface Temperature anomalous weeks in 5 year periods, so the maximum value possible per cell is 260 anomalous weeks. To rescale the values from 0 to 1 we set a reference point. Previously, the reference point for SST has just been the maximum difference in anomalous weeks between the most recent time period and a historical reference period (1985-1989).

This time we have decided to use a reference point that represents a regime shift. Once a given cell is anomalous for more than 50% of a five-year period, it has shifted into a new regime. All cells that have a value greater than 130 weeks (51% of a 5 year time period) are assigned a 1. The rest of the cells are scaled to this reference point by dividing by 130.

-----

## OHIBC: Ultraviolet Anomalies Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_uv_prep.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_uv_prep.html

### Summary:

For Global OHI 2017: 

>the Ultraviolet Radiation pressure layer is generated from daily data on Local Noon Erythemal UV Irradiance (mW/m2) derived from satellite observations. 

>1. Average the data for each week/year/cell  
>2. For each week/year/cell, calculate the mean and sd, so each cell would have ~624 (12*52) values (2004-2016)  
>3. Determine which of these were anomalous, defined as greater than the mean plus 1 standard deviation  
>4. Sum weekly anomalies for each year/cell (for a total of 52 possible anomalies per year/cell)  
>5. Calculate the total number of anomalies in the reference period (in our case, 2004-2009, for a total of 52*5 anomalies per cell) 
>6. Calculate the total number of anomalies in the most recent 5 year period (2011-2015)    
>7. then for each cell, get the difference between current anomalies and reference anomolies    
>8. Rescale the data to be between 0-1 by using the 99.99th quantile as a reference point

For OHIBC, we will use the global prepared data from steps 1-4, then clip to BC regions, interpolate to fill gaps, resample to OHIBC, and determine the pressure based on a reference point of 50% of weeks being higher than the climatological norms (mean + 1 standard deviation).  This 50% reference point represents a threshold to a "new normal" or a regime shift.  

While reference point is based on a five-year mean, annual scores will be based on the year in question only.  We will calculate a score for all years, including the reference point years.


