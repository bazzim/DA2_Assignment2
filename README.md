# DA2_Assignment2

This repository contains folders for assignment for Data Analysis 2 and Coding 1. The main aim of this project is to uncover pattern of association between registered COVID-19 cases and registered number of death due to COVID-19 on a given day.


### Data Folder
Contains the COVID-19 case reports on 17 September 2020, which includes daily confirmed cases, daily death cases, daily recovered cases, and daily active cases from 182 countries and territories.
Daily confirmed cases include confirmed and probable (where reported). Daily death cases include confirmed and probable (where reported). Daily recovered cases are estimates based on local media, reports, and state and local reporting when available, and therefore maybe substantially lower than the true number. Daily active cases are calculated as the total daily confirmed cases minus daily death cases and daily recovered cases.

This dataset is taken from: 
[COVID-19 data link](https://choosealicense.com/licenses/mit/)

Population from year 2019 also included in this data set using the WDI package in R.

### Codes
The codes folder includes the codes to get, clean and analyze the data.

1. get_data.R
Get the raw data for COVID-19 and population, and saves it to data/raw folder

2. clean_data.R
Clean both raw datasets, and merge the datasets based on country. Each observation is a country

3. analysis_covid.R
Analyze and compare different regression models. After selecting the best models, we compare residuals.

### Docs
Contains both HTML and PDF generated using the R Markdown.

