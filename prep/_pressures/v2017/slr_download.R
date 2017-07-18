source('src/R/common.R')

### Get filenames from AVISO FTP URL:
library(RCurl)
url <- "ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/climatology/monthly_mean/"
userpwd <- "nceas_ohara:diors54jh"
filenames <- getURL(url,
                    userpwd = userpwd,
                    ftp.use.epsv = FALSE,
                    dirlistonly = TRUE) %>%
  str_split('\n') %>%
  unlist()

filenames <- filenames[!str_detect(filenames, '.png')]


### Set up loop to download each file and save to git-annex:

ftp_dir <- 'ftp://nceas_ohara:diors54jh@ftp.aviso.altimetry.fr/global/delayed-time/grids/climatology/monthly_mean'
git_anx_dir <- file.path(dir_neptune_data, 'git-annex/globalprep/_raw_data/AVISO_slr/msla_monthly_mean')
for (nc_file in filenames) {
  download.file(url      = file.path(ftp_dir, nc_file),
                destfile = file.path(git_anx_dir, nc_file))
}
