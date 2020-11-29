# Data Analysis 2 and Coding with R
# 
# Date 2020-11-29

library(WDI)
library(tidyverse)


# Get data from raw folder

covid_raw <- read.csv('data/raw/covid_raw.csv')
pop_raw <- read.csv('data/raw/population_raw.csv')


# Clean and merge COVID and Population datasets -------------------------------------------

## Clean covid data

# Check covid data
glimpse(covid_raw)

names(covid_raw)

# Drop unnecessary variables
cv <- covid_raw %>% 
  
  select(-c(FIPS,Admin2,Last_Update,Lat,Long_,Combined_Key,Incidence_Rate,
            Case.Fatality_Ratio))

str(cv)

# One observation to be one country
# Check e.g. China:
cv %>% filter( Country_Region == 'China')

# Create new data table now only contains the countries
cv2 <- cv %>% 
  group_by( Country_Region ) %>% 
  summarise_if(is.numeric,lst(sum))

str(cv2)

head(cv2)

# Rename variables
cv2 <- cv2 %>% rename( country   = Country_Region ,
                       confirmed = Confirmed_sum,
                       death     = Deaths_sum,
                       recovered = Recovered_sum,
                       active    = Active_sum )

str(cv2)

head(cv2)

tail(cv2)

# COVID data was cleaned by:
#   
# 1.Dropping non needed columns, FIPS, Admin2, Last_Update, Lat, Long_, Combined_Key, Incidence_Rate, Case.Fatality_Ratio. This will leave only 6 columns in our data.
# 
# 2.As there are many values for each country, we sum them to be one value for each country.
# 
# 3.We, then, renamed these summed columns
# 
# 4.The final cleaned data will be composed of 188 rows and 5 columns, 
# 
# "country", "confirmed","death","recovered","active"


## Clean population data

head(pop_raw)

table(pop_raw$iso2c)

pop <- pop_raw %>% filter(!grepl("[[:digit:]]", iso2c))

head(pop)

table(pop$iso2c)

# Some grouping observations are still there, check each of them
#   HK - Hong Kong, China
#   OE - OECD members
#   all with starting X, except XK which is Kosovo
#   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe

# 2nd drop specific values
drop_id <- c("EU","HK","OE")

pop %>% filter(iso2c %in% c("EU","HK","OE"))

pop2 <- pop %>% filter(!grepl(paste( drop_id , collapse="|"), iso2c))

head(pop2)

# this will produce the same data frame

pop3 <- pop %>% filter(!iso2c %in% c("EU","HK","OE"))


# 3rd drop values with certain starting char
# Get the first letter from iso2c
fl_iso2c <- substr(pop2$iso2c, 1, 1)

retain_id <- c("XK","ZA","ZM","ZW")

# Filter out everything which starts X or Z except countries in retain_id

pop4 <- pop2 %>% filter(!( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                             !grepl( paste( retain_id , collapse="|"), pop2$iso2c))) 
pop4 %>% filter(iso2c %in% c("XK","ZA","ZM","ZW"))


sort(pop2$iso2c)

pop6<- pop2 %>% filter(!iso2c %in% c("XC","XD","XE","XF","XG","XH",
                                     "XI", "XJ", "XL","XM","XN","XO",
                                     "XP","XQ","XT","XU","XY","ZF","ZG",
                                     "ZJ","ZQ","ZT"))

sort(pop6$iso2c)

str(pop6)

rm( drop_id, fl_iso2c , retain_id )

# Retain and rename variables which are going to be used later
pop7 <-pop6 %>% transmute( country = country,
                           population=SP.POP.TOTL )

str(pop7)

## Clean population data

# Population data was cleaned by:
#   
# 1.Removing iso2c rows containing iso2c with numbers.
# 
# 2.Removing iso2c rows containing characters, "EU","HK","OE".
# 
# 3.Remove iso2c rows which contains iso2c characters that start with X or Z except countries, "XK","ZA","ZM","ZW".
# 
# 4.Create a new data frame containing the country and population columns only. The final data is composed of 216 rows and 2 columns.


## Merge the two data frames

df <- full_join(cv2,pop7)

str(df)

head(df)

tail(df)

# Correct some country names
use_name <- c("Congo, Rep.","Congo, Dem. Rep.","Czech Republic",
              
              "Korea, Rep.","Kyrgyz Republic",  "Laos",
              
              "St. Kitts and Nevis","St. Lucia",
              
              "St. Vincent and the Grenadines",
              
              "Slovak Republic","United States","Myanmar")

alter_name <- c("Congo (Brazzaville)","Congo (Kinshasa)","Czechia",
                
                "Korea, South","Kyrgyzstan","Lao PDR",
                
                "Saint Kitts and Nevis","Saint Lucia",
                
                "Saint Vincent and the Grenadines",
                
                "Slovakia","US","Burma")

df %>% filter(country %in% c("Congo (Brazzaville)","Congo(Kinshasa)",
                             
                             "Czechia","Korea, South","Kyrgyzstan","Lao PDR",
                             
                             "Saint Kitts and Nevis","Saint Lucia",
                             
                             "Saint Vincent and the Grenadines",
                             
                             "Slovakia","US","Burma"))

# Simply use a for loop to change the name for the countries (note: ordering is important)

for (i in seq_along( use_name )){
  
  df$country[df$country == alter_name[i]] <- use_name[i]
}

str(df)

df %>% filter(country %in% c("Congo, Rep.","Congo, Dem. Rep.",
                             
                             "Czech Republic","Korea, Rep.",
                             
                             "Kyrgyz Republic",  "Laos",
                             
                             "St. Kitts and Nevis","St. Lucia",
                             
                             "St. Vincent and the Grenadines",
                             
                             "Slovak Republic","United States","Myanmar")) %>%  data.frame()


# Write a for-loop to find those which are partial matches!
# 1) auxillary data frame for countries without any population value

aux <- df %>% filter(is.na(population))

head(aux)

# 2) Get the name of the countries
countries_nm <- aux$country

# 3) Iterate through all potential partial matches

for (i in seq_along( countries_nm ) ){
  
  # Select those observations where partial match exists
  log_select <- str_detect( df$country , countries_nm[ i ])
  
  # Get the population values for partial matches
  c_partial <- df$population[ log_select ]
  
  # If there is a match: only two countries are selected and one is missing the other has population:
  
  if ( length( c_partial ) == 2 & sum( is.na( c_partial ) ) == 1 ){
    # Replace the missing value with the match
    df$population[ log_select & is.na(df$population)] = c_partial[ !is.na( c_partial ) ]
    # Remove the replaced variable
    df <- df %>% filter( !(log_select & is.na(df$confirmed ) ) )
  }
}

# 4) Check the results:

df %>% filter(is.na(population))

df %>% filter(country=="Kosovo")

# These are:
#   a) cruiser ships which stuck in national territory (Diamond Princess, MS Zaandam )
#   b) disputed territories which are accepted by covid statistics but not by world bank (Western Sahara, Taiwan or Kosovo)
#   c) we have no population data on them (Ertirea, Holy See (Vatican))

#####

# Handle missing values:

df %>% filter(!complete.cases(df)) %>% data.frame()

# Drop if population, confirmed cases or death is missing

df <- df %>% filter(!(is.na(population) | is.na(confirmed) | is.na(death)))


df %>% filter(!complete.cases(df))

# write the data

write_csv(df, "~/Desktop/DA2_Assignment2/data/clean/covid_clean.csv")


# After merging the two data frames, cv2 and pop7, using the common column, country, the resulting data frame was cleaned by:
#   
# 1.Changing some country names.
# 2.Checking for partial matching of country names and correct it.
# 3.The final data is composed of 182 rows and 6 columns.
