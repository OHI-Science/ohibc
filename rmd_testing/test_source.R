### test_source.R

tableA_file <- file.path(dir_test, 'tableA.csv')
tableA      <- read_csv(tableA_file)

tableB_file <- file.path(dir_test, 'tableB.csv')
write_csv(tableA, tableB_file)

tableC_file <- file.path(dir_test, 'tableC.csv')
write_csv(tableA, tableC_file)

tableD_file <- file.path(dir_test, 'table1.csv')
tableD      <- read_csv(tableD_file)
