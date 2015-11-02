dir_git <- '~/github/ohibc'
source(file.path(dir_git, 'src/R/common.R'))

dir_test <- file.path(dir_git, 'rmd_testing')


table1_file <- file.path(dir_test, 'table1.csv')
table1      <- read.csv(table1_file, stringsAsFactors = FALSE)

DT::datatable(table1,   ### does not display system and session info
              caption = 'this is a sample table:',
              rownames = FALSE,
              class = 'stripe hover compact',
              options = list(dom = 'tp'))

table2_file <- file.path(dir_test, 'table2.csv')
write.csv(table1, table2_file)
