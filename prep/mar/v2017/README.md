# Ocean Health Index British Columbia: /prep/mar/v2017

This folder describes the methods used to prepare data for Mariculture for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#food-provision).

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

This directory includes R/Rmd scripts and .html files, as well as subdirectories that include metadata, intermediate data, figures, and output layers for the indicated assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## OHIBC data prep: Aquaculture pressures and resilience

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/mar/v2017/data_prep_aq_prs_res.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/mar/v2017/data_prep_aq_prs_res.html

### Summary:

OHIBC Aquaculture pressures and resilience

This script prepares pressures and resilience layers related to aquaculture production in British Columbia.

-----

## OHIBC data prep: Mariculture

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/mar/v2017/data_prep_mar.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/mar/v2017/data_prep_mar.html

### Summary:

OHIBC Mariculture

This script prepares layers (production potential, area targets, harvest values) for Mariculture sub-goal in British Columbia's coastal regions.  

From Halpern et al. (2012) :

>The Status of the Mariculture sub-goal ($x_{MAR}$), was defined as production of strictly marine taxa from both the marine and brackish water FAO categories, excluding aquatic plants such as kelps and seaweeds, which were assumed to contribute predominantly to medicinal and cosmetic uses rather than as a source of food.

In addition, a sustainability factor is included for the global OHI calculations.

For British Columbia, we will leverage Gentry, Froehlich et al. (2017) to determine aquaculture potential for both shellfish and finfish in BC waters.  For each OHIBC region, the $Phi'$ value based on a generic portfolio of 120 finfish and 60 bivalves is converted into a time-to-harvest value for each 1-km^2 cell.  These in turn are converted to a potential harvest rate $P_c$ as a function of $Phi'$.

To determine a reference point, we will create a rescaled harvest potential similar to the $B' = f(B/B_{MSY})$ score for the FIS subgoal.  A score of 1 will reflect a harvest value greater than $bar{P}(Phi' - sigma_{Phi'})$.  Below the lower bound value, the score tapers linearly to zero (at a harvest of zero) for under-production.  While finfish aquaculture can create environmental problems, we will not impose a sustainability penalty on overproduction as the impact of BC's finfish aquaculture seems to be very low.

The reference point for each region is the product of this rescaled potential harvest rate and the area of existing aquaculture tenures ($A_f$ and $A_s$ for finfish and shellfish respectively) within the region.  Note that MaPP has identified large areas in HG, NC, CC, and NCVI regions for aquaculture, but analogous proposals are not available for WCVI or SG regions.

$$H_{lowR} = bar{P}(Phi' - sigma_{Phi'}) * A_{tenure}$$

where $H_{lowR}$ is calculated separately for finfish and shellfish aquaculture classes.

Mariculture score $x_c$ for harvest $H_c$, calculated separately for each aquaculture class $c$:

| value           |           | condition |
|: |: |: |
| $x_{shellfish} = H/H_{lowR}$ | when | $0 leq H_{shellfish} < H_{lowR}$ |
| $x_{shellfish} = 1$ | when | $H_{shellfish} geq H_{lowR}$ |
| $x_{finfish} = H/H_{lowR}$ | when | $0 leq H_{finfish} < H_{lowR}$ |
| $x_{finfish} = 1$ | when | $H_{finfish} geq H_{lowR}$ |

The Mariculture score will be the harvest-weighted average of $x_{finfish}$ and $x_{shellfish}$.

$$x_{MAR} = frac{x_{finfish} * H_{finfish} + x_{shellfish} * H_{shellfish}}{H_{finfish} + H_{shellfish}}$$


