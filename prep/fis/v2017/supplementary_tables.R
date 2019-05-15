#create supplementary table that includes all stocks, their assessment status (yes/no) and stock id

library(tidyverse)
#ram stocks

all_ram_stocks <- read_csv('~/github/ohibc/prep/fis/v2017/ram/1_bc_stocks_all_ram_no_salmon.csv') %>%
  select(stockid, stocklong, scientificname, commonname)

#just stocks with assessment info
assessed_ram <- bind_rows(read_csv('~/github/ohibc/prep/fis/v2017/output/ram_b_bmsy.csv'),
                          read_csv('~/github/ohibc/prep/fis/v2017/output/ram_f_fmsy.csv')) %>%
  select(stockid = stock_id, scientificname = sciname, stocklong = stock_name) %>%
  distinct() %>%
  mutate(assessed = "yes")

#join

together <- all_ram_stocks %>%
  left_join(assessed_ram) %>%
  mutate(assessed = ifelse(is.na(assessed), "no", "yes"))

write.csv(together, "~/github/ohibc/prep/fis/v2017/output/supp_table_ram_stocks.csv")
