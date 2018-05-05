### Support script for data_prep_le.Rmd


income_empl_1996_file <- file.path(dir_goal, 'int/inc_emp_96.csv')

### 1996 data are saved as a SDMX file for entire Canada
census_1996_gen_file <- file.path(dir_data_anx, 'canada_census',
                               '95F0181XDB96001', 'Generic_95F0181XDB96001.xml')
census_1996_str_file <- file.path(dir_data_anx, 'canada_census',
                               '95F0181XDB96001', 'Structure_95F0181XDB96001.xml')

if(!file.exists(income_empl_1996_file)) {

  ### Seems like the 1996 census file in SDMX format is too large for R
  ### to handle:
  ### Error in readChar(file, file.info(file)$size) : invalid 'nchars' argument
  ### - probably b/c size is 3.2 GB
  # devtools::install_github("opensdmx/rsdmx")
  # census_1996_gen <- rsdmx::readSDMX(census_1996_gen_file, isURL = FALSE)

  ### The process:
  ### * identify which lines have GEO tag;
  ### * from this, get values of the following element (gen_geo_lines + 1)
  ###    * This will be something like value = '<geo code>'
  ### * subset gen_lines to capture only those geo codes that start with 59 (code for BC)
  ### * convert to a data frame for easier working
  ### * then figure out the DIM codes, and do a similar process to identify
  ###    * median household income
  ###    * unemployment rate.

  gen_lines <- scan(census_1996_gen_file, what = 'character')
  gen_geo_lines <- which(str_detect(gen_lines, 'GEO'))
    ### find GEO tag to identify indices of geo codes; slow

  gen_geo_values <- gen_lines[gen_geo_lines + 1]
    ### collect the values for GEO, based on the index following GEO

  gen_geo_vals_bc <- which(str_detect(gen_geo_values, '\"59'))
    ### within the gen_geo_values dataframe, these are the indices within
    ### gen_geo_values that correspond to BC; but these should match up with
    ### the same indices in gen_geo_lines! So:
    ### Use these to identify the indices within gen_geo_lines, which correpond
    ### to the indices in gen_lines.

  gen_lines_bc_indx <- gen_geo_lines[gen_geo_vals_bc]
  # range(gen_lines_bc_indx)
  # 153915966 168043479
  # 168043479 - 153915966 = 14127513
  ### Now if we crop gen_lines down to roughly this neighborhood,
  ### we should be able to work with it a bit more easily.

  gen_lines_bc <- gen_lines[(min(gen_lines_bc_indx) - 100):(max(gen_lines_bc_indx) + 100)]

  gen_lines_bc_df <- data.frame(raw = gen_lines_bc) %>%
    mutate(clean = str_replace_all(tolower(raw), '[^a-z0-9\\.]+', '_')) %>%
    filter(str_detect(clean, '^concept_geo|^concept_dim|obsvalue|^value')) %>%
    mutate(clean = str_replace(clean, 'dim0', 'dim'))
  ### getting rid of the 0 in dim0 helps clean up numeric extraction later...

  ### seems like order is geo, dim, obsvalue, indicating the observed value for that georegion for that dimension.
  ### Widen into observations: sep out the variable names, then get value from
  ###   the following line for that variable instance
  gen_obs_df <- gen_lines_bc_df %>%
    mutate(variable = case_when(str_detect(clean, 'geo')  ~ 'geo_code',
                                str_detect(clean, 'dim') ~ 'dimension',
                                str_detect(clean, 'obsvalue') ~ 'value'),
           val_num  = str_extract(lead(clean), '[0-9\\.]+'),
           geo_code = val_num,
           dim      = lead(val_num, 2),
           value    = lead(val_num, 4)) %>%
    filter(variable == 'geo_code') %>%
    filter(str_detect(geo_code, '^59')) %>%
    select(geo_code, dim, value)

  ### At this point, dataframe of geo_code (location), dimension (what does
  ### the value indicate), and value.  Now need to determine dimensions,
  ### identify the ones of interest, delete the rest, and save it as a csv.

  #####  Now: Crack the codes!
  census_1996_str <- rsdmx::readSDMX(census_1996_str_file, isURL = FALSE)

  ### get codelists from structure
  cls <- slot(census_1996_str, "codelists")

  ### get list of codelists
  codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))
  # "CL_GEO"                 "CL_DIM"           "CL_OBS_STATUS"

  ### get codelist
  geo_codes <- as.data.frame(slot(census_1996_str, "codelists"),
                              codelistId = 'CL_GEO') %>%
  setNames(c('geo_code', 'csd_name')) %>%
  mutate(geo_code = as.numeric(geo_code))

  char_codes <- as.data.frame(slot(census_1996_str, "codelists"),
                                       codelistId = 'CL_DIM') %>%
  setNames(c('dim', 'xx', 'characteristic')) %>%
  select(-xx) %>%
  mutate(dim = as.numeric(dim))

  inc_empl_codes <- char_codes %>%
  filter(str_detect(tolower(characteristic), 'income|unempl'))
  ### 1614: Household income of all private households (20% sample data)
  ### 1627: median household income for all households
  ###  797: Total population 15 years and over by labour force activity (20% sample data)
  ###  804: Unemployment rate

  ##### NOW: connect the generic file with the codes file... filter to the
  ### household income and unemployment lines
  inc_empl_bc_1996 <- gen_obs_df %>%
    mutate(geo_code = as.numeric(geo_code),
           dim      = as.numeric(dim),
           value    = as.numeric(value)) %>%
    left_join(geo_codes, by = 'geo_code') %>%
    left_join(char_codes, by = 'dim') %>%
    filter(dim %in% c(804, 1627)) %>%  ### income and unemployment dimensions
    filter(geo_code > 10000) %>%       ### cut to census subdistricts, with high geo_codes
    mutate(field = ifelse(str_detect(characteristic, 'Unempl'), 'unempl_rate', 'med_income'),
           year  = 1996) %>%
    select(csd_id = geo_code, csd_name, year, field, value)

  write_csv(inc_empl_bc_1996, income_empl_1996_file)
}
