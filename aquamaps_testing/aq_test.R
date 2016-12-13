library(rfishbase)
# devtools::install_github("raquamaps/raquamaps")
library(raquamaps)
library(dplyr)
library(tidyr)
library(stringr)

fb <- fishbase     ### dataframe built into package
slb <- sealifebase ### dataframe built into package

datasets <- data(package = "raquamaps")$results[ ,"Item"]
print(datasets)
#  [1] "aquamaps_galemys_pyrenaicus"   "aquamaps_hc"                   "aquamaps_hcaf_eu"
#  [4] "aquamaps_hcaf_world"           "aquamaps_presence_basins"      "aquamaps_presence_occurrences"
#  [7] "aquamaps_qc"                   "aquatic_hcaf"                  "rgbif_galemys_pyrenaicus"
# [10] "rgbif_great_white_shark"

hcaf <- aquatic_hcaf ### 259200 obs of 132 vars - is this all the env data???
hcaf2 <- raquamaps::aquamaps_hc ### 259200 obs of 8 vars -
  ### CsquareCod | LOICZID | NLimit | Slimit | WLimit | ELimit | CenterLat | CenterLong
  ### note: no cell area info

names_fb <- fb %>%
  unite(sciname, Genus:Species, sep = ' ')

am_sids_list <- vector('list', length = nrow(names_fb))
for(i in 1:length(am_sids_list)) {
  sciname <- names_fb$sciname[i]
  message(i, '. Checking for data on ', sciname)
  ids <- get_am_name_uris(sciname)
  if(length(ids) >= 1) {
    message('Found data for ', sciname, '!')
    test[i] <- ids
    names(test)[i] <- sciname
  }
}

am_sid_df <- data.frame(am_sid = unlist(test),
                        sciname = names(test)[!is.na(names(test))],
                        stringsAsFactors = FALSE)

write_csv(am_sid_df, 'aquamaps_testing/am_sid_fb_df.csv')



