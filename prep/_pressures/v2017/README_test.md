# Ocean Health Index British Columbia: /prep/_pressures/v2017

This folder describes the methods used to prepare data for Pressures for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

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

## OHIBC: Genetic Escapes Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_gen_escapes.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/pressures_gen_escapes.html

### Summary:

This pressure layer calculates the pressure due to genetic escapes from aquaculture.  The Mariculture Sustatinability Index ranks aquaculture species in different regions with a score from 1-10 for potential for genetic pollution.  For OHIBC we will follow a similar method as OHI Global and CC, rescaling genetic escape pressure from 0-1 for each species and calculating a harvest-weighted average of genetic escape pressure for each region.

$$pressure_{gen.escapes} = frac{sum_{spp=1}^N (MSI_{spp} * harvest_{spp})}{sum_{spp=1}^N harvest_{spp}}$$

The genetic escapes scores are not time-dependent, but harvest data is reported annually.

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

For OHIBC these pressure layers are read in and cropped to BC extents for pressure calculations.

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

There are two parts to creating this layer:

1. Data prep to get raw data into the correct format:
    * Read anomalies data as raster for 1997-2001 and 2010-2014.
    * Clip rasters to extents of OHIBC region, and reproject into BC Albers projection.
2. Creating the pressure layers for OHIBC:
    * Determine change in anomalies since baseline.
    * Set negative values to zero (only interested in increases).
    * Resample to 1 km.
    * Log transform results.
    * Interpolate to coastline at 1 km resolution.
    * Rescale zero to 1, where 1 is 99.99%ile
    
This process is completed entirely within this script.


