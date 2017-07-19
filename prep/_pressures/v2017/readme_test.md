# Ocean Health Index British Columbia: /prep/_pressures/v2017

<!--This folder describes the methods used to prepare data for _GOALNAME_ for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#artisanal-fishing-opportunities).

-->

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

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
    * Read sea level rise data as raster
    * Clip rasters to extents of OHIBC region, and reproject into BC Albers projection.
2. Creating the pressure layers for OHIBC:
    * All negative values, indicating decreases in sea level, were set to zero
    * Data was resampled from the native cell resolution (0.25 degrees) to ~ 1km
    * The reference point was set as the 99.99th quantile of the data distribution to rescale all values from 0 to 1
    * All NA cells were filled in through nearest neighbor interpolation
    
This process is completed entirely within this script.

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

-----

## OHIBC: Sea Level Rise Pressure layers prep

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/_pressures/v2017/pressures_slr_prep2.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/_pressures/v2017/NA

### Summary:

There are two parts to creating this layer:

1. Data prep to get raw data into the correct format:
    * If necessary, read .nc.gz files from aviso: ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/climatology/monthly_mean/
        * unzip .gz files and then delete .gz 
    * Read monthly mean sea level data as rasters; aggregate to annual means
    * Clip rasters to extents of OHIBC region, and reproject into BC Albers projection.
2. Creating the pressure layers for OHIBC:
    * Determine baseline using five-year average from 1993-1997.
    * For each study year (2000 - 2015) calculate difference between study year and baseline
    * Set to zero all negative values, indicating decreases in mean sea level
    * Resample data from the native cell resolution (0.25 degrees) to ~ 1km
    * Set reference point as the 99.99th quantile of the data distribution to rescale all values from 0 to 1
    * Use thin plate spline interpolation to fill in NA values
    * Clip results to 1 km offshore region, and determine average SLR pressure per region.
    
This process is completed entirely within this script.


