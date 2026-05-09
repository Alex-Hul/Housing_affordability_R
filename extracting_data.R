library(dplyr)
library(tidyr)
#base period is 2015 for datasets we analyse

prices_income <- read.csv("/Users/alexandr/Desktop/Studying/Intro_Stats_Project_Data/prices_income_unfiltered.csv")
summary(prices_income) #this df contains real house prices and average price of house to income ratio (as indexes)
head(prices_income) #base period is 2015

#raw df, need to filter

clean <- filter(prices_income, FREQ == "A") %>% #we do not need quarterly data
  filter(Measure == "Price to income ratio" | Measure == "Real house price indices") %>% #variables of interest
  
  mutate(year = as.numeric(TIME_PERIOD), int_var = OBS_VALUE, country = Reference.area, code = REF_AREA) %>% 
  
  filter(year >= 1995) #most of the countries have data from this period
  #aggregates: EA17, EA, OECD
  
prices_and_income <- clean[, c("year", "code", "country", "Measure", "int_var")] %>%
  pivot_wider(names_from = Measure, values_from = int_var) %>%

  rename(price_to_income = "Price to income ratio", real_house_prices = "Real house price indices")

View(prices_and_income)


cleaned <- function(df){ #this function is for filtered datasets from OECD, which will be used below
  clean <- df %>% 
    filter(FREQ == "A") %>%
    mutate(year = as.numeric(TIME_PERIOD), country = Reference.area, code = REF_AREA) %>% 
    filter(year >= 1995) 
  
  df2 <- clean[, c("year", "code", "country", "Measure", "OBS_VALUE")] %>%
    pivot_wider(names_from = Measure, values_from = OBS_VALUE)
  
  return(df2)
}

int_rates <- read.csv("/Users/alexandr/Desktop/Studying/Intro_Stats_Project_Data/short_long_int_rates.csv")
#this df contains short- and long-term interest rates
summary(int_rates)
#View(int_rates)

interest_rates <- cleaned(int_rates) %>%
  rename(short_int_rates = "Short-term interest rates", long_int_rates = "Long-term interest rates")

View(interest_rates)

subsidies <- read.csv("/Users/alexandr/Desktop/Studying/Intro_Stats_Project_Data/Housing_GDP_part.csv")
#this df contains government subsidies on housing as percent of GDP (Measure is social expenditure, but program is housing)
summary(subsidies)

housing_subsidies <- cleaned(subsidies) %>%
  rename(housing_subsidies = "Social expenditure")

View(housing_subsidies)

gdp <- read.csv("/Users/alexandr/Desktop/Studying/Intro_Stats_Project_Data/Real_gdp.csv") %>%
  rename(Measure = Transaction)
#this df contains Real GDP (chain linked volume) as index with 2020 as the base period. So it is needed to be rebased.
r_gdp <- cleaned(gdp) %>%
  rename(real_gdp_index = "Gross domestic product")
#rebasing

real_gdp <- r_gdp %>%
  group_by(country) %>%
  
  mutate(val_2015 = ifelse(any(year == 2015), real_gdp_index[year == 2015], NA),
         real_gdp_index = real_gdp_index*100/val_2015 ) %>%
  
  ungroup() %>%
  select(-val_2015)
  
View(real_gdp)


aggr_df <- prices_and_income %>%
  left_join(interest_rates, by = c("year", "code", "country")) %>%
  left_join(housing_subsidies, by = c("year", "code", "country")) %>%
  left_join(real_gdp, by = c("year", "code", "country"))

View(aggr_df)

#a lot of NA - we will analyse OECD countries with enough data
analysed_countries = c("Australia", "Austria", "Belgium", "Canada", "Chile", 
                       "Czechia", "Denmark", "Estonia", "Finland", 
                       "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", 
                       "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", 
                       "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", 
                       "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States") 

final_df = filter(aggr_df, country %in% analysed_countries)

View(final_df)
