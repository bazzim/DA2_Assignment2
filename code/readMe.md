# Code

This folder contains codes for the analysis of the pattern of association between registered COVID-19 cases and registered number of deaths due to COVID-19 on 17 September 2020.


### get_data.R
Get the raw data for COVID-19 and population, and saves it to data/raw folder

### clean_data.R
Clean both raw datasets, and merge the datasets based on country. Each observation is a country

### analysis_covid.R
Analyze and compare different regression models. After selecting the best models, we compare residuals.