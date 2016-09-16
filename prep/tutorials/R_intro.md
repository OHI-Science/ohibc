## R Tutorials for OHI

Ocean Health Index R code uses several packages and best practices to faciliate
understanding and collaboration. These approaches are presented here, along with
examples using data included in global OHI assessments.

This document describes several packages that are used extensively in OHI assessments and 
introduces you to typical coding practices commonly seen in OHI scripts and functions.

Also see the accompanying R script to test examples using these packages. 

### R Very Basics:
* Have you already downloaded and installed [R](http://www.r-project.org/)?
* Have you already downloaded and installed [RStudio](http://www.rstudio.com/)?
* Have you walked through the excellent interactive tutorials
from [swirl](http://swirlstats.com/students.html)?

### `tidyr` functions

'Tidy' up your messy data using `tidyr` to make it easier to work with.  The
'tidy tools' functions in the `dplyr` package work best with tidy data.

From Hadley Wickham's [*Tidy Data* paper:](http://vita.had.co.nz/papers/tidy-data.html)
>It is often said that 80% of data analysis is spent on the cleaning and preparing data. And it's not just a first step, but it must be repeated many over the course of analysis as new problems come to light or new data is collected. To get a handle on the problem, this paper focuses on a small, but important, aspect of data cleaning that I call data tidying: structuring datasets to facilitate analysis.

From [RStudio's introduction to `tidyr`](http://blog.rstudio.org/2014/07/22/introducing-tidyr/):

> The two most important properties of tidy data are:
1. Each column is a variable.
2. Each row is an observation.

> Arranging your data in this way makes it easier to work with because you have a
consistent way of referring to variables (as column names) and observations
(as row indices). When you use tidy data and tidy tools, you spend less time
worrying about how to feed the output from one function into the input of
another, and more time answering your questions about the data.


`gather()` is arguably the most useful function in `tidyr`, and is explained in
more detail below.  `spread()` and `separate()` are other useful functions in
`tidyr`.

Other 'tidyr' references:
* [Hadley Wickham's *Tidy Data* paper:](http://vita.had.co.nz/papers/tidy-data.html)
Download the pre-print version for the whys and hows of tidy data.
* [Cran tidy data vignette:](http://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html)
An informal and code heavy version of Hadley's full *Tidy Data* paper.
* [RStudio Blogs: Introducing tidyr:](http://blog.rstudio.org/2014/07/22/introducing-tidyr/)
Basics and philosophy of `tidyr`
* [swirl tutorial package:](http://swirlstats.com/students.html) A tutorial
package built directly into R.  Section 2: 'Getting and Cleaning Data' runs you
through `dplyr` and `tidyr` basics
* [R data wrangling cheat sheet:](http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf)
A quick reference guide to `tidyr` and `dplyr` functions

#### tidyr::gather()

**Description**  

`gather()` takes data organized in rows and collapses them into a column format (a
key column and a value column), duplicating all other columns as needed. Use
`gather()` when your data is organized in "wide" format, in which some of your
variables are in row form, rather than column form.  Another `tidyr` function,
`spread()`, is more or less the reverse of `gather()`, to reformat long data
into wide data. It is more difficult to work with wide data, but may be
more convenient for examining data in a table format.

Note: `gather()` essentially replaces `melt()` in `plyr` package.

**Example**  

The sample data set (see intro) contains harvest data of a number of marine
commodities, separated by country, commodity, and year.  In its original form,
the harvest data (in tonnes) is spread across five different harvest years.
* Counter to 'tidy data' principles, we have multiple columns (X2007:X2011)
representing a single variable (year), and multiple observations of harvest
tonnage in each row.
* To transform this into 'tidy data' we will gather the five annual harvests
into a single column called 'tonnes' and note the year of harvest in a new column
called 'year'.

In this example (see figure), the original wide data is transformed into long
data using the command:

```
data_long <- data_wide %>% gather(year, tonnes, X2007:X2011)
  ### Gathers columns X2007 through X2011 into a single column called 'year';
  ### the values from each column are put into a new column called 'tonnes'.

data_long <- data_wide %>% gather(year, tonnes, -Country, -Commodity, -Trade)
  ### Same results; the '-' unselects the named columns, so they will not
  ### be gathered; all other columns are gathered into 'year' and 'tonnes'.
```

![wide data to long data using gather() and spread()](https://docs.google.com/drawings/d/1VaZdLWK0NwAkov4sEytZLRpOUAndb3_NZOA4-n1HNIo/pub?w=948&h=499)

### `dplyr` functions

The `dplyr` package includes a number of functions to easily, quickly, and
intuitively wrangle your data. Here is a quick introduction with examples from data used in the Ocean Health Index.

From [RStudio's introduction to `dplyr`](http://blog.rstudio.org/2014/01/17/introducing-dplyr/):

> The bottleneck in most data analyses is the time it takes for you to figure
out what to do with your data, and `dplyr` makes this easier by having individual
functions that correspond to the most common operations...

> Each function does one only thing, but does it well.

The most important `dplyr` functions to understand for data processing will be
 `group_by()`, `mutate()`, and `summarize()`. Also important, `dplyr` introduces
the ability to perform subsequent functions in a logical and intuitive manner,
using the `%>%` chain operator.

* `%>%` (chaining operator): allows sequential
chaining of functions for cleaner, easier-to-read code
* `dplyr::select()`: selects variables to be retained or dropped from dataset
* `dplyr::filter()`: filters data set by specified criteria
* `dplyr::arrange()`: sorts dataset by specified variables
* `dplyr::mutate()`: adds variables or modifies existing variables
* `dplyr::summarize()`: uses analysis functions (sum, mean, etc) to summarize/aggregate specified variables
* `dplyr::group_by()`: groups data by specified variables, allowing for group-level data processing.

Other `dplyr` references:

* [RStudio blogs: Introducing dplyr:](http://blog.rstudio.org/2014/01/17/introducing-dplyr/): philosophy, examples, and basics of `dplyr`
* [Cran dplyr vignette:](http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html) Walkthrough of `dplyr` with examples
* [`dplyr` and pipes: the basics:](http://seananderson.ca/2014/09/13/dplyr-intro.html) More examples of `dplyr` functions, and more depth on `%>%`
* [swirl tutorial package:](http://swirlstats.com/students.html) A tutorial package built directly into R.  Section 2: 'Getting and Cleaning Data' runs you through `dplyr` and `tidyr` basics
* [R data wrangling cheat sheet:](http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf) a quick reference guide to `tidyr` and `dplyr` functions

#### %>% operator

**Description**  

The `%>%` operator allows you to 'pipe' or 'chain' a number of function calls,
in which the output dataframe of one function is fed directly into the next
function as the input dataframe.
This lets you avoid creating temporary variables to store intermediate values,
and lets you avoid nesting multiple functions.  Using `%>%` makes your code more elegant, streamlined, and easy to read since you are able to write your code on multiple indented lines.  From
[`dplyr` and pipes: the basics:](http://seananderson.ca/2014/09/13/dplyr-intro.html)  

> OK, here's where it gets cool. We can chain `dplyr` functions in succession.
This lets us write data manipulation steps in the order we think of them and
avoid creating temporary variables in the middle to capture the output. This
works because the output from every `dplyr` function is a data frame and the
first argument of every `dplyr` function is a data frame.

**Usage**  

```
data_out <- f(data_in, args)
  # standard function call

data_out <- data_in %>% f(args)
  # function call using %>% operator. data_in is passed as first argument
  # of function().

data_out <- data_in %>%
  f1(args1) %>%
  f2(args2) %>%
  f3(args3) %>% ...
  # Output of function can be passed to another function immediately,
  # without need for temporary storage. Indented format for legibility,
  # see how pretty it looks?
```

**Example**  

```
### Bad!  Nested functions: read from inside out - hard to decipher
  h_recent_totals1 <- arrange(mutate(filter(group_by(harvest, country, commodity),
    year >= 2009), harvest_tot = sum(tonnes, na.rm = TRUE)), country, commodity)

### Better: Line by line. Easier to read, but have to wait for the end to see
### what it does.  Temp variables add more places for errors and bugs.
  h_temp <- group_by(harvest, country, commodity)
  h_temp <- filter(h_temp, year >= 2009)
  h_temp <- mutate(h_temp, harvest_tot = sum(tonnes, na.rm = TRUE))
  h_recent_totals2 <- arrange(h_temp, country, commodity)

### Best!  Chained format intuitively links together the functions. Saves
### typing, fewer opportunities for errors, easier to debug. The %>% operator
### automatically indents each following line for easy reading.
  h_recent_totals3 <- harvest %>%
    group_by(country, commodity) %>%
    filter(year >= 2009) %>%
    mutate(harvest_tot = sum(tonnes, na.rm = TRUE)) %>%
    arrange(country, commodity)
```

#### dplyr::select()

**Description**  

`select()` allows you to choose specific columns/variables from your dataset,
and drop all others.  Alternately, you can select specific variables to drop,
leaving others in place.  `rename()` is a relative of `select()` that allows
you to rename variables, while leaving all variables in place.


**Examples**  

The sample dataset  includes the annual harvest, in tonnes, of a number of
commodities exported by two countries.  Type of trade provides no information
(it is all Export), so that variable can be dropped.  The names of all the
variables should be converted to lower-case, to match the OHI style guide.
See the figure below.

```
### Example 1:
harvest <- harvest %>%
  select(Country, Commodity, year, tonnes)
  ### Selects the named variables, and drops all others.  Useful to choose a
  ### subset of key variables from a complicated data set.

### Example 2 (same result as example 1):
harvest <- harvest %>%
  select(-Trade)
  ### Using the '-' drops 'Trade' column and leaves other variables intact.
  ### Useful if you would like to clear out temporary variables.

harvest <- harvest %>%
  rename(country = Country, commodity = Commodity)
  ### Drops no variables. Syntax: rename(new_var_name = old_var_name) w/o quotes.
```

Using the chain operator, we can string these two functions into one smooth,
easy-to-read flow:
```
harvest1 <- harvest %>%
  select(-Trade) %>%
  rename(country = Country, commodity = Commodity)
```
The `harvest` data is fed into `select()`, and the output is fed into
`rename()`. The final output of this complete flow is assigned to the new
variable `harvest1`.
![using select() and rename() to organize variables in a data set](https://docs.google.com/drawings/d/14uc-1Pgaosfh5kPllJRf_sRXbiGWL4RcBqASqAG5f2E/pub?w=898&h=286)

#### dplyr::filter()

**Description**  

`filter()` allows you to select observations (rows) that match search criteria,
using values in specified variables (columns).  Drops all observations that do
not match the criteria.
* Use logical operators & and | to filter on multiple criteria simultaneously

**Example**  

```
harvest_vnm  <- harvest %>%
  filter(country == 'Vietnam')
  ### Single criterion filter: keeps only data with country matching 'Vietnam'.

h_vnm_recent <- harvest %>%
  filter(country == 'Vietnam' & year >= 2009)
  ### filter with multiple criteria: selects 'Vietnam' data from 2009 or later.
```

#### dplyr::arrange()

**Description**  

`arrange()` sorts observations (rows) based upon a specified variable or list of
variables.  Does not actually change the data in any way, only the appearance.
Useful for inspecting your data after each processing step.

**Example**  

```
harvest_sorted <- harvest %>%
  arrange(country, commodity, year)
  ### Sorts commodity harvest values for each country, chronologically

harvest_sorted <- harvest %>%
  arrange(country, commodity, desc(year))
  ### Sorts harvest values by most recent year (descending order)
```

#### dplyr::mutate()

**Description**  

`mutate()` is a powerful and useful tool for processing data.  You can add new
variables or modify existing variables, using all variety of functions to
perform operations on the dataset. `mutate()` works well with `group_by()` to
perform calculations and analysis at a group level rather than dataset level.

**Example**  

From the sample data set (see figure below), we would like to:

* Remove the 'X' from the 'year' values.
* Translate the text codes in 'tonnes' into numbers and NAs.  These codes are
specific to FAO's data reporting format: `...` is the same as `NA`, and `0 0`
means greater than zero, but less than half a tonne.
* Convert these text fields into numeric fields so they can be analyzed properly.

![Using mutate to alter data in a dataframe](https://docs.google.com/drawings/d/1LbBLBM7dI8TP8cCknT-VDqY-SW5ceBkfnkZUlftG8lo/pub?w=889&h=297)

```
library(stringr)   ### to access 'str_replace()' string functions

harvest1 <- harvest %>%
  mutate(  
    year   = str_replace(year,   fixed('X'),    ''),  # remove the 'X'
    tonnes = str_replace(tonnes, fixed('...'),  NA),  # replace '...' with 'NA'
    tonnes = str_replace(tonnes, fixed('0 0'), 0.1),  # replace '0 0' with '0.1'
    tonnes = ifelse(tonnes =='', NA, tonnes)) %>%
  mutate(
    tonnes = as.numeric(as.character(tonnes)),
    year   = as.integer(as.character(year)))
```
Notes:
* In this example, no new variables were added. Multiple variables can be
changed with one call to `mutate()`. Multiple modifications to 'tonnes' happen
sequentially, so order is important.
* The `as.numeric(as.character(...))` gets around the fact that these text
variables are stored as 'factor' class, rather than 'character' class.
`as.character()` forces them into character class, and then the `as.numeric()`
can convert the character strings to numeric where applicable. Similar for
`as.integer(...)`


#### dplyr::summarize() ( or summarise() )

**Description**  

`summarize()` combines multiple values of a variable into a single summary
value. `summarize()` works well with `group_by()` - for grouped data, each
group will be summarized and reported separately. For ungrouped data, the
summary covers the entire dataset.

* `summarize()` compresses the dataset and drops individual observations. To
maintain individual observations, consider creating a summary variable using
`mutate()` instead.
* `NA` values can be problematic - use `na.rm=TRUE` or similar methods.

**Example**  

To determine the total harvest of each country, for each commodity:
```
h_summary <- harvest %>%
  group_by(country, commodity) %>%
  summarize(harvest_tot = sum(tonnes, na.rm = TRUE)) %>%
  ungroup()
```


#### dplyr::group_by()

**Description**  

`group_by()` allows you to easily group a dataset by one or more variables/columns.  
By itself, it does nothing to change your data.  But once your dataset has
been sorted into useful groups, other `dplyr` functions will operate on each
group separately, rather than operating on the entire dataset.
* The function `groups(data)` reports back the current grouping status of
dataframe `data`.  

* `group_by()` alters the grouping, but does not alter the sort order.  
`arrange()` does not alter the current grouping - it will sort by groups
first, then sorts within each group.
* Multiple calls to `group_by()` will reset the groupings each time (by
  default), rather than adding additional layers of groups.
* Once you have finished with your operation at the group level, it is a good
practice to use the `ungroup()` function to remove the groupings, to avoid
unintended consequences due to forgotten `group_by()` calls.

**Example**  

If you want to find the total tonnage harvested for each commodity for each
country, you would want to group by country and by commodity, and then perform
a `sum()` function on the grouped data.  Two options presented here:
`summarize()` to collapse data to just the summary, and `mutate()` to add a
new column that includes the summary values.
```
h_tot_sum <- harvest %>%
  group_by(country, commodity) %>%
  summarize(harvest_tot = sum(tonnes, na.rm = TRUE))
    ### Summarize information by collapsing each group to a single summary value
    ### (total tonnage by commodity by country). Note ungroup() at end.

h_tot_mut <- harvest %>%
  group_by(country, commodity) %>%
  mutate(harvest_tot = sum(tonnes, na.rm = TRUE)) %>%
  arrange(country, commodity) %>%
  ungroup()
    ### Summarize information by creating a new variable to contain summary
    ### value; report value for every observation. Note ungroup() at end.
```
![group_by to find group-level information](https://docs.google.com/drawings/d/1enHrgXWhpHz3FsURncMI5UB8LKoXLvXFPAcU25pDOSc/pub?w=745&h=285)

### Coding style 

> Code unto others as you would have them code unto you.

Why style? ask Hadley Wickham, developer of many wonderful R packages:

> Good style is important because while your code only has one
author, it’ll usually have multiple readers. This is especially true when you’re
writing code with others. In that case, it’s a good idea to agree on a common
style up-front. Since no style is strictly better than another, working with
others may mean that you’ll need to sacrifice some preferred aspects of your style.

The Ocean Health Index is founded upon principles of open-source science, so our code should be not just available, but legible to others.  For OHI+, we expect people to modify code to implement new goal models, and we may need to provide support in developing and debugging their code.

Certain coding techniques are more efficient than others (e.g. in R, looping across elements in a vector is much slower than operating on the entire vector at once), but rarely does OHI code push any performance envelopes.  Much more of our time is spent writing code, translating old code into new models, and debugging.  Transparent, readable code will save more time in the future than a perfectly-optimized but opaque algorithm.  

Readable code is:

* collaborative
* easier for others to understand and debug
* easier for others to update and modify
* easier for 'future you' to interpret what 'past you' meant when you wrote that chunk of code.

Check out Hadley Wickham's [style guide: ](http://r-pkgs.had.co.nz/style.html)

* How many of these suggestions are second-nature to you? how many are you guilty of breaking?
* Note that these are guidelines, not rules; non-stylish code can still work.  

#### Best practices for coding in OHI assessments:

* use a consistent format for variable names, filenames, function names, etc.
    * `lower_case_with_underscores` (preferred) or `camelCase` (ok I suppose)
        * not `periods.in.between`
    * use names that are brief but intuitive
* Comment clearly for your own purposes, and for others.
    * Comment on the purpose of each important block of code.
    * Comment on the reasoning behind any unusual lines of code, for example an odd function call that gets around a problem.
* Take advantage of R Studio section labels functionality:
    * If a comment line ends with four or more -, =, or # signs, R Studio recognizes it as a new section.
    * Text within the comment becomes the section name, accessible in the drop-down menu in the bottom left of the RStudio script window.
* use <- to assign values to variables (not necessary, but preferred)
* use %>% to create intuitive chains of related functions
    * one function per line
    * break long function calls into separate lines (e.g. multiple mutated variables)
* use proper spacing and formatting for legibility
    * don't crowd the code - use spaces between math operators and after commas
    * use indents to indicate nested or sequential/chained code
    * break sequences or long function calls into separate lines logically -
    e.g. one function call per line
* use functions to add intuitive names to chunks of code
* Use 'tidy data' practices - take advantage of `tidyr`, `dplyr`
    * clean up unused columns using `select(-colname)`
* if you are working on an older script, spend a few extra minutes to update it according to these best practices
    * technical debt - you can do it quickly or you can do it right.  Time saved now may cost you or someone else more time later.

#### Writing functions
http://nicercode.github.io/guides/functions/
Why write functions?
* name a chunk of code for easier reading
* easily reuse a chunk of code

What makes a good function:
* It’s short
* Performs a single operation
* Uses intuitive names

#### Directories and files
    * Store files in a folder called 'github' in your home directory; access it with `~/github` so that users with different operating systems can work smoothly with your files

