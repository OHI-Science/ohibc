library(readr)
library(data.table)
library(dplyr)
library(seaaroundus)
library(raster)
library(rgdal)


# Path to data
source('~/github/ohibc/src/R/common.R')
dir_anx <- file.path(dir_M, 'git-annex/bcprep')
path_data <- '/home/shares/ohi/git-annex/globalprep/_raw_data/SAUP/d2016/Data'
file_alloc_data    <- 'SeaAroundUs/AllocationData.dat'
file_alloc_results <- 'SeaAroundUs/AllocationResult.dat'
file_taxon  <- 'SeaAroundUs/taxon.dat'
file_entity <- 'FishingEntity.dat'
dir_spatial <- file.path('~/github/ohibc/prep/spatial') %>%
  path.expand()

# load the allocation info: allocation of each species per year
data_dt <- fread(file.path(path_data, file_alloc_data),
                 sep=';', showProgress = TRUE,
                 header = FALSE)
colnames(data_dt) <- c('UniversalDataID', 'DataLayerID', 'FishingEntityID', 'Year', 'TaxonKey',
                       'InputTypeID', 'sector_type_name', 'catch_type_name',
                       'reporting_status_name')

# load the Results data: for each UDI, allocated catch per cell
results_dt <- fread(file.path(path_data, file_alloc_results),
                    sep=';', showProgress = TRUE,
                    header = FALSE)
colnames(results_dt) <- c('UniversalDataID', 'CellID', 'AllocatedCatch')

### Find cells for BC EEZ
# bc_rgn <- readOGR(dir_spatial, 'ohibc_rgn') %>%
#   spTransform(CRS('+init=epsg:4326'))
# bbox(bc_rgn)
# # min        max
# # x -138.75908 -122.75493
# # y   46.52686   55.93538
bc_cells <- getcells('POLYGON ((-139 46, -139 56, -122 56, -122 46, -139 46))')
### 735 cells

setkey(results_dt, UniversalDataID)
setkey(data_dt,    UniversalDataID)

results_bc_dt <- results_dt[CellID %in% bc_cells]
data_bc_dt    <- data_dt[UniversalDataID %in% results_bc_dt$UniversalDataID]

saup_bc_dt <- data_bc_dt[results_bc_dt]

write_csv(results_bc_dt, file.path(dir_anx, 'fis/v2017/saup/results_bc.csv'))
write_csv(data_bc_dt,    file.path(dir_anx, 'fis/v2017/saup/data_bc.csv'))
write_csv(saup_bc_dt,    file.path(dir_anx, 'fis/v2017/saup/saup_bc.csv'))


yearlist <- unique(data_bc_dt$Year) %>% sort()

# get template raster for SAUP data with cell values equal to CellID

bc_saup_rast   <- raster(ncol = 35, nrow = 21,
                         xmn = -139, xmx = -121.5, ymn = 46, ymx = 56.5,
                         resolution = .5)
bc_saup_rast[] <- bc_cells


for(year in yearlist) {
  # year <- yearlist[1]
  message('working on year ', year)
  data_year_dt <- data_bc_dt[Year==year, ] ### keep as data.table

  #subset the data for 10 unique UniversalDataID
  udi <- unique(data_year_dt$UniversalDataID)

  #subset the allocation, results
  j <- saup_bc_dt %>%
    filter(UniversalDataID %in% udi)

  #groupby/summarize
  grouped_data <- group_by(j, CellID)
  summed_data <- grouped_data %>%
    summarize(val = sum(AllocatedCatch))
  out_rast <- raster::subs(bc_saup_rast, summed_data, subsWithNA = TRUE)

  out_name <- file.path(dir_anx, 'fis/v2017/saup', paste(year, 'catch.tif', sep = '_'))
  writeRaster(out_rast, out_name, overwrite = TRUE)
  plot(out_rast)

}
