library(rfishbase)
# devtools::install_github("raquamaps/raquamaps")
library(raquamaps)

source('src/R/common.R')

fb <- fishbase     ### dataframe built into package
slb <- sealifebase ### dataframe built into package

datasets <- data(package = "raquamaps")$results[ ,"Item"]
print(datasets)
#  [1] "aquamaps_galemys_pyrenaicus"   "aquamaps_hc"                   "aquamaps_hcaf_eu"
#  [4] "aquamaps_hcaf_world"           "aquamaps_presence_basins"      "aquamaps_presence_occurrences"
#  [7] "aquamaps_qc"                   "aquatic_hcaf"                  "rgbif_galemys_pyrenaicus"
# [10] "rgbif_great_white_shark"

# hcaf <- aquatic_hcaf ### 259200 obs of 132 vars - is this all the env data???
# hcaf <- raquamaps::aquamaps_hc ### 259200 obs of 8 vars -
### CsquareCod | LOICZID | NLimit | Slimit | WLimit | ELimit | CenterLat | CenterLong
### note: no cell area info

names_fb <- fb %>%
  unite(sciname, Genus:Species, sep = ' ')

# for(j in 1:4) { ### j = 3 as a page index
#   scinames_j <- names_fb$sciname[((j - 1) * 10000 + 1):(j * 10000)]
#   scinames_j <- scinames_j[!is.na(scinames_j)]
#   am_sids_list <- vector('list', length = length(scinames_j))
#
#   for(i in 1:length(scinames_j)) { ### i <- 12
#     sciname <- scinames_j[i] ### [10000 + i] for other
#     message(i, '. Checking for data on ', sciname)
#     ids <- get_am_name_uris(sciname)
#     if(length(ids) >= 1) {
#       message('Found data for ', sciname, '!')
#       am_sids_list[i] <- ids
#     } else {
#       am_sids_list[i] <- 'no ID found'
#     }
#
#     names(am_sids_list)[i] <- sciname
#
#   }
#
#   am_sid_df <- data.frame(am_sid = unlist(am_sids_list),
#                           sciname = names(am_sids_list)[!is.na(names(am_sids_list))],
#                           stringsAsFactors = FALSE)
#
#   write_csv(am_sid_df, sprintf('aquamaps_testing/am_sid_fb_df_%s.csv', j))
# }

sciname_vec <- names_fb$sciname

### get the whole am_sid list for all scinames
library(parallel)

chunksz <- 1000

for(j in 1:(length(sciname_vec)/chunksz + 1)) {
  sciname_vec_j <- sciname_vec[((j-1) * chunksz + 1):j*chunksz]
  am_sids_list <- mclapply(sciname_vec_j,
                           FUN = function(x) {
                             y <- get_am_name_uris(x)
                             y_l <- length(y)
                             z <- data.frame(sciname = x,
                                             am_sid  = ifelse(y_l >= 1, y, 'no ID found'),
                                             am_sid_count = y_l,
                                             stringsAsFactors = FALSE)
                             ### added in a 2-second pause between requests, as a courtesy to the AquaMaps server
                             Sys.sleep(2.0)
                           }, mc.cores = 12)

  ### NOTE: some requests ended up with a 'try-error' response; eliminate these
  ### for now, and go back and re-try later on? (!sciname %in% sciname_vec)

  am_sids_list2 <- am_sids_list[sapply(am_sids_list, class) != 'try-error']
  am_sid_df <- bind_rows(am_sids_list2)
  write_csv(am_sid_df, sprintf('aquamaps_testing/am_sid_fb_df_%s.csv', j))
}
