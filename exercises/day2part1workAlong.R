library(tidyverse)

setwd("~/repositories/r_tutorial/bootcamp-2019/data")

generation <- read_csv("ca_energy_generation.csv") #can also do (here::here(data/"file name")) if didn't have data as WD
imports <- read_csv("ca_energy_imports.csv")

#if hadn't read dates and times properly, could have used:
library(lubridate)
generation$datetime <- as_datatime(generation$datetime)


#make in long: in our data, each observation should be date-energy source
library(reshape2) #in tidyverse
long_gen <- melt(generation, 
                 id.vars = 'datetime',
                 variable.name = 'source',
                 value.name = 'usage')

#merge generation and imports
merged_energy <- merge(generation, imports, by = 'datetime')
dim(merged_energy) #returns dimensions of df

long_merged_energy <- melt(merged_energy,
                            id.vars = 'datetime',
                            variable.name = 'source',
                            value.name = 'usage')

#DPLYR

#select 
select(merged_energy, biogas, biomass, geothermal, solar) %>% head()
select(merged_energy, biogas, biomass, geothermal, solar) %>% names()

#select helpers  
select(merged_energy, contains("hydro"), starts_with("bio")) %>% names()

#filter
merged_energy %>% nrow()
merged_energy %>% filter(imports > 7000) %>% nrow()  
merged_energy %>% filter(imports > 7000, natural_gas < 7000) %>% nrow()  

#mutate
long_merged_energy %>% mutate(log_usage = log(usage)) %>% head()
long_merged_energy %>% mutate(log_usage = log(usage), usage2 = usage^2) %>% head()

#summarize
summarize(long_merged_energy, mean_cons = mean(usage, na.rm = T))
long_merged_energy %>% summarize(mean_cons = mean(usage, na.rm = T))

#piping

long_merged_energy %>% #this pipe returns the mean_log_usage for only geothermal
  filter(source == "geothermal") %>% 
  select(-datetime) %>% #deselect datetime
  mutate(log_usage = log(usage)) %>% 
  summarize(mean_log_usage = mean(log_usage, na.rm = T)) 

merged_energy %>%
  select(contains('hydro')) %>% 
  mutate(total_hydro = rowSums(., na.rm = T)) %>% # '.' in rowSums supplies df from previous step to rowSums function 
  summarize(mean_usage = mean(total_hydro, na.rm = T))


# Group by -- alternative to using for loops
long_merged_energy %>%
  group_by(source) %>%
  summarize(sum_usage = sum(usage, na.rm = T))
  

#find mean usage for small hydro, large hydro, biogas and biomass

long_merged_energy %>%
  filter(source == 'small_hydro' | source == 'large_hydro' | source == 'biogas' | source == 'biomass') %>%
  # filter(source %in% c('small_hydro', ....))
  group_by(source) %>%
  summarize(mean_usage = mean(usage, na.rm = T))

#merge vs joins

merge(generation, imports, by = "datetime", all = F) %>% dim()
inner_join(generation, imports, by = "datetime") %>% dim()

##############
##DATA TABLE##
##############

library(data.table)

data_file <- here::here("data/ca_energy_generation.csv")

# read in two versions of data, one as a data.frame and one as a data.table
generation_df <- read.csv(data_file, stringsAsFactors = F) 

generation_dt <- fread(data_file)

class(generation_df)
class(generation_dt)

View(generation_df)
View(generation_dt)
generation_df
generation_dt #the print function in data table is better
str(generation_df)
str(generation_dt)

#filter
generation_dt[wind > 4400]
generation_dt[wind > 4400 & mday(datetime) == 7]

generation_dt[natural_gas <= 5000 & large_hydro > 2000]
generation_dt[coal > 10 & solar > median(solar)]

#what to do (after first comma)

generation_dt[,wind + solar]

#new variable
generation_dt[, 3*wind + solar*biogas/2]  #jsut prints it
generation_dt[, newcol := 3*wind + solar*biogas/2] #adds new column
generation_dt[, .(newcol = 3*wind + solar*biogas/2)] #export to new df 

generation_dt[, newcol := NULL] #delete column


##exercises

generation_dt[, total_hydro := small_hydro + large_hydro]
generation_dt[, mean_nuke_bio := (nuclear + biogas)/2]
generation_dt[solar == 0, .(datetime, total_thermal = natural_gas + coal)] %>% head() #filter solar = 0, select datetimer, total_thermal, 

#group by
#find mean of nuclear by day
generation_dt[,mean(nuclear), by = mday(datetime)]
generation_dt[,.(mean_nuke = mean(nuclear), mean_wind = mean(wind)), by = mday(datetime)] #use list with dot to show multiple or group by multiple vars

#find median solar by hour
generation_dt[,median(solar), by = hour(datetime)]

#dfind max natural gas by day when solar is greater than 0
generation_dt[solar > 0, max(natural_gas), by = mday(datetime)]
generation_dt[solar > 0, max(natural_gas), mday(datetime)]

#convert dplyr to data.table
long_ca_energy <- long_ca_energy %>%
  mutate(day = as_date(datetime),
         log_output = log(output)) %>%
  group_by(day) %>%
  mutate(total_daily_output = sum(output, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(per_output = output/total_daily_output)

long_merged_energy_dt = as.data.table(long_merged_energy)

long_merged_energy_dt[,day := as_date(datetime)]
long_merged_energy_dt[,log_output := log(usage)]
long_merged_energy_dt[,per_output := usage/sum(usage), by = day]

#special vars
generation_dt[,.N] #number rows
generation_dt[solar > 0, .N] #number rows with solar
generation_dt[solar > 0, .I] #lists teh rows

imports_dt <- fread(here::here("data", "ca_energy_imports.csv"))

imports_dt

#joins / merge

imports_dt[generation_dt, on = "datetime"] #merge funcion from base R is compatible 







