# Ocean Health Index R code uses several packages and best practices to faciliate
# understanding and collaboration. These approaches are presented here, along with
# examples using data included in global OHI assessments.


### setup ----

# The Ocean Health Index (OHI) team uses certain packages extensively. 
# Being familiar with these packages will be useful in interpreting
# existing OHI code and in developing code for modified and updated goal models. 
# The developer of these packages, Hadley Wickham, says, "structuring datasets to facilitate analysis".  
# For more information, see http://had.co.nz/
#      - Use `readr` to read in data files of different types **including csv files that use ';' as separators**
#      - Use `stringr` to work with strings and characters
#      - Use `tidyr` to organize your data into 'tidy data' format
#      - Use `dplyr` to filter, calculate, and 'wrangle' your tidy data


# if not already installed, install these packages (OHI favorites):
package_list <- c('readr', 'stringr', 'tidyr', 'dplyr')
for (p in package_list) {
 if (!require(p, character.only = TRUE)) 
   install.packages(p)
}

# once installed, you only need to attach the packages from the library:
library(readr)
library(stringr)
library(tidyr)
library(dplyr)

# set working directory: conduct all work in a folder called 'github' in your home directory
setwd('~/github/ohibc/region2016/prep/tutorials')

# read in data
# - `commodities_sample.csv` is a truncated version of Fishery Commodities Global Production and Trade data 
#    (http://www.fao.org/fishery/statistics/global-commodities-production/en) from the FAO
# - `comm2prod_sample.csv` is a lookup table created by the OHI team. 

data_wide <- read.csv('commodities_sample.csv') # harvest data on marine commodities (FAO)
lookup    <- read.csv('comm2prod_sample.csv')   # lookup table to assign individual commodities into product groups (OHI)


### chaining with the %>% operator----

# The `%>%` operator allows you to 'pipe' or 'chain' a number of function calls so that the output dataframe 
# of one function is fed directly into the next function as the input dataframe.
# This allows you to avoid creating temporary variables to store intermediate values,
# and lets you avoid nesting multiple functions.  Using `%>%` makes your code more elegant, streamlined, 
# and easy to read since you are able to write your code on multiple indented lines.  

# See http://seananderson.ca/2014/09/13/dplyr-intro.html for more description, 
# and below for further examples


### tidyr examples ----

# 1. gather data: transform data from wide format (where some variables are contained in rows, rather than columns) 
#    into long format (where each column is a variable, and each row is a single observation).
data_long <- data_wide %>%
  gather(year, tonnes, -Country, -Commodity, -Trade)
    # Note that the (-Country, -Commodity, -Trade) arguments tell the function
    # to gather all except those rows while gathering, and instead repeat 
    # these values for each of the observations in the new 'year' column.

# The same result could be achieved with this alternative function call:
data_long1 <- data_wide %>%
  gather(year, tonnes, X2007:X2011)

# Note that you might get a warning message: "attributes are not identical across measure variables;
# they will be dropped".  If you inspect the original data_wide table, some columns ('X2009') are all
# integer values, and others ('X2008') contain a mix of numbers and letters (e.g. '...' or '0 0').  
# R tries to figure out what type of data is in each column, but when faced with complex data, it has to
# resort to more flexible data classes, e.g. character.


# 2. spread data (less commonly used): transform data from long format (columns as variables) into wide
#    format (rows as variables)
data_wide_again <- data_long %>%
  spread(year, tonnes)

harvest <- data_long


### dplyr examples ----

# 1. select and rename columns
harvest <- harvest %>% 
  select(-Trade) %>% 
  rename(country = Country, commodity = Commodity)

# 2. mutate the dataset by changing values in dataset.
#    This example also uses str_replace(), a function in the `stringr` package.

harvest <- harvest %>%
  mutate(
    year   = str_replace(year, fixed('X'), ''),      
    tonnes = str_replace(tonnes, fixed('...'),    NA),
    tonnes = str_replace(tonnes, fixed('0 0'), 0.1),
    tonnes = ifelse(tonnes =='', NA, tonnes)) %>%
  mutate(
    tonnes = as.numeric(as.character(tonnes)),
    year   = as.integer(as.character(year)))
      # looks odd, but required because these values are stored as 'factor'
      # class rather than 'character' class.... as.character() forces factor
      # into character, and then as.integer() and as.numeric converts
      # characters into integers/numbers where possible.


# 3. summarize information by specific grouping. 
#    Collapse each group to just a single summary value (total tonnage by commodity by country), 
#    first using group_by() to summarize harvest information by commodity for each country. 
#    When done, it's a good idea to use ungroup() so further operations act on individual rows.
h_tot_sum <- harvest %>%
  group_by(country, commodity) %>%
  summarize(harvest_tot = sum(tonnes, na.rm = TRUE)) %>%
  ungroup()

# 4. summarize information by creating a new variable to contain summary value; report value for every observation
h_tot_mut <- harvest %>%
  group_by(country, commodity) %>%
  mutate(harvest_tot = sum(tonnes, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(country, commodity)

# 5. arrange information in descending order
harvest_reverse <- h_tot_mut %>%
  arrange(desc(year), country, desc(commodity))
    # sorted by year newer-to-older, and then within each year 
    # by country in alpha order, then within each country/year
    # by commodity in reverse alphabetical order.


### chaining (%>% operator) examples ---

# Turn these ugly nested functions...
h_recent_totals <-arrange(mutate(filter(group_by(harvest, country, commodity),
  year >= 2009), harvest_tot = sum(tonnes, na.rm = TRUE)), country, commodity)

# ...and this boring and unintuitive sequence of functions...
h_temp <- group_by(harvest, country, commodity)
h_temp <- filter(h_temp, year >= 2009)
h_temp <- mutate(h_temp, harvest_tot = sum(tonnes, na.rm = TRUE))
h_recent_totals1 <- arrange(h_temp, country, commodity)

# ...into glorious chained functions:
h_recent_totals2 <- harvest %>%
  group_by(country, commodity) %>%
  filter(year >= 2009) %>%
  mutate(harvest_tot = sum(tonnes, na.rm = TRUE)) %>%
  arrange(country, commodity)
