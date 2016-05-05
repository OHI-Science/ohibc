
table1_file <- file.path(dir_test, 'table1a.csv')
table1      <- read_csv(table1_file)

table2_file <- file.path(dir_test, 'table2.csv')
table2      <- read_csv(table2_file)

table3_file <- file.path(dir_test, 'table3.csv')
table3      <- read_csv(table3_file)

tableD_file <- file.path(dir_test, 'tableD.csv')
write_csv(table1, tableD_file)

tableE_file <- file.path(dir_test, 'tableE.csv')
write_csv(table2, tableE_file)

