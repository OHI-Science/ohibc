### ? Put an R Markdown header up here, and save this file as a .Rmd instead of .R

### RMarkdownPractice.R 
### sample script to practice converting R into R Markdown. 

##############################################################################.
### ? Include a brief description up here at the top: use a header, write
###   write some text briefly describing what this script is doing.

### Header suggestion: NAFTA Natural Products harvest
### Text suggestion: This script investigates the harvest of Natural Products for the Ocean
###   Health Index, for the three NAFTA member countries: US, Canada, and Mexico.
### Include a link to this website: (http://news.bbc.co.uk/2/hi/business/4510792.stm)
### Include this map: (http://newsimg.bbc.co.uk/media/images/41104000/gif/_41104266_nafta_416map.gif)

##############################################################################.
### ? turn these pieces (divided by the horizontal bar) into separate R code 
###   chunks.  For Setup, use echo = FALSE to hide the code; try setting
###   message = TRUE and message = FALSE to see the difference when loading
###   packages.
### Setup 
### load libraries, source function files, set pathnames, etc

### load libraries
library(tidyr)
library(dplyr)
library(stringr)
 
### set directory paths
dir_git  <- '~/github/ohibc'
dir_data <- file.path(dir_git, 'data')
setwd(dir_git)

### access functions specific to this tutorial
source('RMarkdown_fxn.R')


##############################################################################.
### ? Use a header and formatted text to give a brief description for what
###   this code chunk is doing.  Try bold, italic, and code-formatted text.
###   Set echo, message, and warning as desired.
###   The source of the data is: http://www.fao.org/fishery/statistics/software/fishstatj/en
### Read and tidy raw data set

### Read in dataset for quantity (tonnes) of harvest. 
harvest_filename <- file.path(dir_data, 'FAO_raw_commodities_quant_1950_2011.csv')

message(sprintf('\nReading FAO Commodity file: %s', basename(harvest_filename)))
harvest_raw <- read.csv(harvest_filename, check.names = FALSE, strip.white = TRUE, stringsAsFactors = FALSE)

### Determine units based on file name (from original code, which loops over
### two data sets: quantity in tonnes, and value in US dollars)
units <- c('tonnes', 'usd')[str_detect(harvest_filename, c('quant', 'value'))]

### Rename variables and gather harvest values from all years into one 
### column indexed by year (long format), and remove unused rows
harvest_data <- harvest_raw %>% 
  rename(country   = `Country (Country)`,
         commodity = `Commodity (Commodity)`,
         trade     = `Trade flow (Trade flow)`) %>%
  gather(year, value, -country, -commodity, -trade)

harvest_data <- harvest_data %>% 
  filter(!country %in% c('Totals', 'Yugoslavia SFR')) 

##############################################################################.
### Clean up data for Natural Products use: 
###   clean codes, assign product groups, and divide up the Antilles into 
###   separate regions.

### Clean up the FAO-specific codes into values useful for OHI
harvest_data <- harvest_data %>% fao_clean_data()

harvest_data <- harvest_data %>%
  select(-trade) %>% 
  arrange(country, commodity, is.na(value), year)

### Use lookup table to assign product groups according to commodity name
message('Loading and joining commodity-to-product lookup table.\n')
comm_to_prod <- read.csv(file.path(dir_git, 'data/commodities2products.csv'), na.strings = '')
harvest_data <-harvest_data %>% 
  inner_join(comm_to_prod, by = 'commodity')

harvest_data <- harvest_data %>% np_fix_antilles()

##############################################################################.
### Prepare data frame to be saved as .csv:
###   rename "value" column, set directory, write it, and print as a table.

### Rename the 'value' column to the appropriate units
names(harvest_data)[names(harvest_data) == 'value'] <- units

harvest_filename <- sprintf('%s/harvest_%s.csv', dir_data, units)

message(sprintf('Writing cleaned harvest data to %s.\n', harvest_filename))
write.csv(harvest_data, harvest_filename, row.names = FALSE, na = '')

print(head(harvest_data)) ### ? turn this into a formatted table with knitr::kable()

##############################################################################.
### Examine NAFTA nation data by table and by graph

### Filter to just NAFTA nations, and aggregate by product group
nafta_list <- c('United States of America', 'Canada', 'Mexico')
h_prod_nafta <- harvest_data %>% 
  filter(country %in% nafta_list) %>%
  group_by(country, product, year) %>%
  summarize(tonnes = sum(tonnes, na.rm = TRUE))

print(head(h_prod_nafta)) ### ? turn this into an interactive table with DT::datatable()

cat(sprintf('The US harvest of fish oil in %s was %s tonnes.\n', max(h_)))

### Compare fish oil harvests across all three NAFTA countries
h_fishoil <- h_prod_nafta %>%
  filter(product == 'fish_oil')

### ? This is commented out for quicker knitting...
# h_plot_fishoil <- ggplot(data = h_fishoil, aes(x = year, y = tonnes, color = country)) +
#   theme(axis.ticks = element_blank(),
#         text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
#         plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
#         legend.position = 'right') +
#   geom_line() +
#   labs(title = 'Fish oil harvest in NAFTA countries')
#   
# h_plot_fishoil ### ? need to wrap this in 'print()' for the plot to show up in the knitted R Markdown...

h_max <- which(h_fishoil$tonnes == max(h_fishoil$tonnes))

### ? convert this next bit into formatted text, using inline R code:
cat(sprintf('The maximum harvest of fish oil was %s tonnes in %s, by %s.\n',
            h_fishoil$tonnes[h_max], h_fishoil$year[h_max], h_fishoil$country[h_max]))


### Compare harvests of each Natural Products product group for US
h_prod_us <- h_prod_nafta %>%
  filter(str_detect(tolower(country),'united states'))

### ? This is commented out for quicker knitting...
# h_plot_prod_us <- ggplot(data = h_prod_us, aes(x = year, y = tonnes, color = product)) +
#   theme(axis.ticks = element_blank(),
#         text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
#         plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
#         legend.position = 'right') +
#   geom_line() +
#   scale_y_log10() +
#   labs(title = 'United States harvest of natural products',
#        ylab  = 'tonnes (log scale)')
# 
# h_plot_prod_us
