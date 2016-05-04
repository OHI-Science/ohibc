
table1_file <- file.path(dir_test, 'table1.csv')
table1      <- read_csv(table1_file)

table1a_file <- file.path(dir_test, 'table1a.csv')
write_csv(table1, table1a_file)


