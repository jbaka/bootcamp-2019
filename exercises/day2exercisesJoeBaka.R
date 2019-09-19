setwd("~/repositories/r_tutorial/bootcamp-2019/data")
library(tidyverse)
library(data.table)
library(lubridate)

data_file_gen <- 'ca_energy_generation.csv'
data_file_imp <- 'ca_energy_imports.csv'

gen <- fread(data_file_gen)
imp <- fread(data_file_imp)

data <- inner_join(gen, imp, by = 'datetime') %>%
  melt(id.vars = 'datetime',
       variable.name = 'source',
       value.name = 'usage') %>%
  as.data.table()

data[, day := as_date(datetime)]
data[, log_usage := log(usage)]
data[, per_usage := usage/sum(usage, na.rm = T), by = day]

mean_hourly_dt <- data[,.(mean_hourly = mean(usage)), by = source]
mean_hourly_dt[mean_hourly == max(mean_hourly) | mean_hourly == min(mean_hourly)] 

data_daily <- data[usage > 0, .(daily_usage = mean(usage)), by = .(source, day(datetime))]
mean_daily_dt <- data_daily[,.(mean_daily = mean(daily_usage)), by = source]
mean_daily_dt[mean_daily == max(mean_daily) | mean_daily == min(mean_daily)] 










