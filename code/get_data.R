# Data Analysis 2 and Coding with R
# 
# Date 2020-11-29


library(WDI)
library(tidyverse)

# Data sources ------------------------------------------------------------

### Download the raw data and examine them

# Download COVID cross-sectional data for the assigned date
date <- '09-17-2020'
covid_url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/',
                    date,'.csv')

covid_raw <- read.csv(covid_url)

# Covid data is composed of 3955 observations and 14 variables
#These 14 columns are:

"FIPS" "Admin2" "Province_State" "Country_Region" "Last_Update"

"Lat" "Long_"  "Confirmed" "Deaths" "Recovered" 

"Active"  "Combined_Key" "Incidence_Rate"  "Case.Fatality_Ratio"

glimpse(covid_raw)

head(covid_raw)

tail(covid_raw)

# write the raw covid data to csv file
write_csv(covid_raw, "~/Desktop/DA2_Assignment2/data/raw/covid_raw.csv")


# download population data
pop_raw <- WDI(indicator=c('SP.POP.TOTL'), 
               country="all", start=2019, end=2019)

# The population data is composed of 264 observations and 4 variables
# These 4 columns are:
#   "iso2c" "country"     "SP.POP.TOTL" "year" 

glimpse(pop_raw)

head(pop_raw)

tail(pop_raw)


# write the raw population data to csv file

write_csv(pop_raw, "~/Desktop/DA2_Assignment2/data/raw/population_raw.csv")


