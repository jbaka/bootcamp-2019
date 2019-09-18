library(tidyverse)

data <- read_csv('gapminder5.csv')

# return statemtn if any records from 2002; 2012
year <- 2002
if(any(data$year == year)) {
    print(paste('there are records for', year))
} else {
    print(paste('there are no records for', year))
}

year <- 2012

if(any(data$year == year)) {
    print(paste('there are records for', year))
} else {
    print(paste('there are no records for', year))
}

##

overall_mean <- mean(gapminder$pop)

for (i in unique(gapminder$country)) {
    country_mean <- mean(gapminder$pop[gapminder$country==i])
    
    if (country_mean < overall_mean) {
        mean_le <- mean(gapminder$lifeExp[gapminder$country==i])
        print(paste("Mean Life Expectancy in", i, "is", mean_le))
    } 
}
                 