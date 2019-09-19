data_schools <- as.data.table(read_csv('data/nys_schools.csv'))
data_counties <- as.data.table(read_csv('data/nys_acs.csv'))

data_schools[per_free_lunch == -99, per_free_lunch = NA]

