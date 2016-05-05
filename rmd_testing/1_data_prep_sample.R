
tableC_file <- file.path(dir_test, 'tableC.csv')
tableC      <- read_csv(tableC_file)

table3_file <- file.path(dir_test, 'table3.csv')
write_csv(tableC, table3_file)

table2_file <- file.path(dir_test, 'table2.csv')
write_csv(tableA, table2_file)

source(file.path(dir_test, '1a_gapfill.R'))
