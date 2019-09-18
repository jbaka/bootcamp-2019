library(here)
gapminder <- read.csv(here::here("data/gapminder5.csv"), stringsAsFactors=FALSE)
gapminder$continent <- as.character(gapminder$continent)
gapminder$country <- as.character(gapminder$country)
str(gapminder)



#create vector values that you want to repeat the function for
obs <- 1:nrow(gapminder) # obs is the vector [1,...,n] where n is the number of rows in gapminder

for (i in obs) { #initialize the for loop 
    gapminder[i, 'gdp'] <- gapminder[i, 'pop'] * gapminder[i, 'gdpPercap'] #assign the operation to var gdp in obs 'i' 
}

#create new var that finds the natural log of the GDP pc and of population, call them log_gdpPercap and log_pop

for (i in obs) {
    gapminder[i, 'log_gdpPercap'] <- log(gapminder[i, 'gdpPercap']) #square bracket innards are location in gapminder of value. row i, column 'gdpPercap'
    gapminder[i, 'log_pop'] <- log(gapminder[i, 'pop'])
}

#alternative: vecotrized function

gapminder$vec_log_gdpPercap <- log(gapminder$gdpPercap)
all(gapminder$vec_log_gdpPercap == gapminder$log_gdpPercap) #test that both for loop and vectorized function methods return same results

#calculate average life expectency per year
years <- unique(gapminder$year) #create vector of years in dataset

for (i in years) {
    mean_le <- mean(gapminder$lifeExp[gapminder$year == i], #calculate avg LE for each year in vector year
                     na.rm = TRUE) #strip NA values
    print(paste0(i, ': ', mean_le)) #every time for loop runs, overwrite old value, so we print at the end of each loop
}
    

#find mean life exp per continent

continents <- unique(gapminder$continent)

for (i in continents) {
    mean_le <- mean(gapminder$lifeExp[gapminder$continent == i], na.rm = T)
    print(paste0(i, ': ', mean_le))
}

# nested for loops

for (i in continents) {
    print(paste0('Continent: ', i))
    for (j in years) {
        mean_le <- mean(gapminder$lifeExp[gapminder$continent == i &
                                              gapminder$year == j],
                        na.rm = T)
        print(paste0(j, ': ', mean_le))
    }
}

#has the gap in life expectancy among countries on  continents narrowed over time

for (i in continents) {
    print(paste0("Continent: ", i))
    for (j in years) {
        sd_le <- sd(gapminder$lifeExp[gapminder$continent == i & 
                                          gapminder$year == j], 
                    na.rm = T)
        print(paste0(j, ": ", sd_le))
    }
}
        

#alternative: apply 

# find the mean for each stat in gapminder

vars <- gapminder[, c('lifeExp', 'pop', 'gdpPercap')] #define set of values we are interested in in gapminder, [, means all rows
apply(vars, 2, mean) #apply mean over columns (2=cols, 1=rows). Hence, we get average for each column in matrix 'vars'

#doing the same in a for loop

for (i in vars) {
    print(mean(i))
}

#apply lets us apply over rows or columsn. lapply and sapply allow us to do operations over values in data frams. 
#lapply returns a list. sapply returns a simplified list 

lapply(gapminder, mean) #applies the mean to each variable/column in gapminder
sapply(gapminder, mean)

sapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x]))

# while loops--like a for loop, but applies until some condidtion is no longer met 

i <- 1952 #define the interator
 
while (i < 1987) {
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ': ', sd_le))
    i <- i + 5 #increase the iterator by the interval between years *5 in our ds
}

i <- 1987

while (i <= 2002) {
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ': ', sd_le))
    i <- i+5 #if forget this, will have infinite loop, b/c infinite values between 1987 and 2002
}
    
    
# if then conditionals
random_year <- sample(years, 1)

random_year

if (random_year > 1977) {
    print(random_year)
} else {
    print("sorry, random year is less than or equal to 1977")
}

#which continentrs have mean life expectency greater than 70 years

threshold <- 70

for (i in unique(gapminder$continent)) {
    tmp <- mean(gapminder$lifeExp[gapminder$continent == i])
    
    if (tmp < threshold) {
        print(paste('Mean life expectency in', i, 'is less than', threshold))
    } else {
        print(paste('Mean life expectency in', i, 'is less than', threshold))
    }
}
    
    
#Write a for loop that reports the mean population for years ??? 1987. Make wure the loop prints a message if the condition is not met



for (i in years) {
    if (i >= 1987) {
        mean_pop <- mean(gapminder$pop[gapminder$year == i])
        print(paste('Mean population in', i, 'is', mean_pop))
    } else {
        print(paste(i, 'is earlier than 1987'))
    }
}
    
    
#function

gapminder$lifeExp[gapminder$country == 'Germany']
`[`(gapminder$lifeExp, gapminder$country == "Germany")   #identical. the latter is a function

fun1 <- 
    function(x,y) {
    }

#simple function that prints the value of a selected variable in the gapminder dataset
get_values <- 
    function(df, variable = 'continent') { #arguments data_frame and variable
        vals <- unique(df[[variable]]) #binds the unique values of the dataframe 
        print(paste0(variable, ': ', vals))
    }
    
get_values(gapminder)


#print mean and se for le for country in gapminder

report_mean_sd <- 
    function(df, variable, country) {
        var <- df[[variable]][df$country == country] #double brackets is an argument [[column]] equiv to [,column] | for all rows in df, column var, choose thoes for which country = country supplied
        m_le <- mean(var)
        ad_le <- sd(var)
        cat('Country:', country,
            '\nMean Life Expectancy:', m_le,
            '\nSD Life Expectancy:', sd_le)
    }
 report_mean_sd(gapminder, 'lifeExp', 'Bulgaria')
 
 
 # write a function that reports the mean, median, minimum and maximum for life expeftancy for a continent in gapminder
 
 report_summary <- 
     function(continent) {
         var <- gapminder[['lifeExp']][gapminder$continent == continent] #define var as life expecency where continetn = continent
         meanLE <- mean(var)
         medianLE <- median(var)
         minLE <- min(var)
         maxLE <- max(var)
         cat('Continent:', continent,
             '\nMin life expectancy:', minLE,
             '\nMax life expectancy:', maxLE,
             '\nMean life expectancy:', meanLE,
             '\nMedian Life expectancy:', medianLE)
     }

report_summary('Africa')
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    