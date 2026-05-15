library(dplyr)
library(tidyr)
library(ggplot2)

##################### PART 1: DATA EXTRACTION

#base period is 2015 for datasets we analyse

prices_income <- read.csv("/Users/alexandr/Desktop/Studying/Intro_Stats_Project_Data/prices_income_unfiltered.csv")
summary(prices_income) #this df contains real house prices and average price of house to income ratio (as indexes)
#This df contains also a lot of different indexes (unfiltered)
head(prices_income) #base period is 2015

#raw df, we need to filter

clean <- filter(prices_income, FREQ == "A") %>% #we do not need quarterly data
  filter(Measure == "Price to income ratio" | Measure == "Real house price indices") %>% #variables of interest
  
  mutate(year = as.numeric(TIME_PERIOD), int_var = OBS_VALUE, country = Reference.area, code = REF_AREA) %>% 
  
  filter(year >= 1995) #most of the countries have data from this period
  #comment for me: aggregates: EA17, EA, OECD
  
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
#this df contains Real GDP (chain linked volume) as index with 2020 as the base period. So it needs to be rebased.
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

#Now, it is time to merge all datasets. The first dataset is the main with dependent variables
aggr_df <- prices_and_income %>%
  left_join(interest_rates, by = c("year", "code", "country")) %>%
  left_join(housing_subsidies, by = c("year", "code", "country")) %>%
  left_join(real_gdp, by = c("year", "code", "country"))

View(aggr_df)

#a lot of NAs - we will analyse OECD countries with enough data
analysed_countries = c("Australia", "Austria", "Belgium", "Canada", "Chile", 
                       "Czechia", "Denmark", "Estonia", "Finland", 
                       "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", 
                       "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", 
                       "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", 
                       "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States") 

final_df = filter(aggr_df, country %in% analysed_countries)

View(final_df)


##################### PART 2: DATA ANALYSIS

summary(final_df) #summary statistics: min, max, quantiles, NAs, etc.

colnames(final_df)

#now, let us analyse the development of key variables throughout last 25-30 years
target_countries <- c("United States", "United Kingdom", "Canada", 
                      "Australia", "France", "Germany", "Japan", "Italy")

trend_df <- filter(final_df, country %in% target_countries)
View(trend_df)

#Below are 2 graphs with the evolution of each of the main variables

house_prices_evol <- ggplot(trend_df, aes(x = year, y = real_house_prices, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.6) +
  labs(
    title = "Evolution of Real House Prices",
    subtitle = "Index: 2015 = 100",
    x = "Year", y = "Real House Price Index",
    color = "Country") +
  theme_minimal()

print(house_prices_evol)

price_inc_evol <- ggplot(trend_df, aes(x = year, y = price_to_income, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.6) +
  labs(
    title = "Evolution of House Price to Income Ratio",
    subtitle = "Index: 2015 = 100",
    x = "Year", y = "House price to income ratio",
    color = "Country") +
  theme_minimal()

print(price_inc_evol)

#Here is the evolution of subsidies, small graph for each of the countries
subsidies_evol <- ggplot(trend_df, aes(x = year, y = housing_subsidies)) +
  facet_wrap(~country, ncol=4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.6) +
  labs(
    title = "Evolution of Housing Subsidies",
    x = "Year", y = "Housing subsidies, percent of GDP") +
  theme_minimal()

print(subsidies_evol)



top_10_2021 <- housing_subsidies %>% #creating dataset to visualise in bar chart
  filter(year == 2021) %>%
  filter(!is.na(housing_subsidies)) %>%  
  arrange(desc(housing_subsidies)) %>%   
  head(10)                        

# next is bar chart
bar_chart_subsidies <- ggplot(top_10_2021, aes(x = reorder(country, housing_subsidies), y = housing_subsidies, fill = housing_subsidies)) +
  geom_col() + 
  coord_flip() + 
  scale_fill_viridis_c(option = "mako") + 
  labs(
    title = "Top 10 Spenders on Housing Subsidies (2021)",
    subtitle = "Government support as a percentage of GDP",
    x = "Country", y = "Subsidies (% of GDP)" ) +
  
  theme_minimal() +
  theme(legend.position = "none")

print(bar_chart_subsidies)

#histogram
data_2021 <- housing_subsidies %>% #dataset for the histogram
  filter(year == 2021) %>%
  filter(!is.na(housing_subsidies))

# and the histogram
subsidies_histogram <- ggplot(data_2021, aes(x = housing_subsidies)) +
  geom_histogram(
    bins = 10,                
    fill = "lightblue3", 
    color = "black",           
    alpha = 0.8) +
  
  labs(
    title = "Distribution of Housing Subsidies in 2021",
    subtitle = "Showing how many countries fall into different spending brackets",
    x = "Housing Subsidies (% of GDP)",
    y = "Number of Countries (Frequency)"
  ) +
  theme_minimal()

print(subsidies_histogram)

sd(final_df$housing_subsidies, na.rm = TRUE)

afford_boxplot <- ggplot(final_df, aes(x = reorder(country, price_to_income, FUN = median, na.rm = TRUE), y = price_to_income)) +
  
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.6) +
  
  coord_flip() + #because the genious idea of showing all countries with their lables works only in this way
  
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", linewidth = 0.8) +
  #it is to see the variance from the base value 100
  labs(
    title = "Price to income ratio ranked by median",
    x = "country",
    y = "Price to income ratio"
  ) +
  
  theme_minimal()

print(afford_boxplot)

############ CORRELATIONS 

corr_matrix <- cor(final_df[, c("real_house_prices", "price_to_income",   "short_int_rates",
                 "long_int_rates",    "housing_subsidies", 
                 "real_gdp_index")], use="complete.obs") # a few NA crash the correlation

round(corr_matrix, digits=4)

cat(0.7472^2, 0.4354^2, 0.3651^2, 0.1990^2) # To write R^2 in the plots


# Correlation between the real GDP and real house prices
prices_gdp_corr <- ggplot(final_df, aes(x = real_gdp_index, y = real_house_prices)) +
  geom_point(color = "blue4", alpha=0.6) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation is 0.7472, R^2 = 0.5583",
       x = "real GDP (chain linked volume, index)",
       y = "Real house prices (index)") +
  theme_minimal()

print(prices_gdp_corr)


# Correlation between the real GDP and price to income ratio
price_inc_gdp_corr <- ggplot(final_df, aes(x = real_gdp_index, y = price_to_income)) +
  geom_point(color = "green4", alpha=0.6) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation is 0.4354, R^2 = 0.1896",
       x = "real GDP (chain linked volume, index)",
       y = "House price to income ratio (index)") +
theme_minimal()

print(price_inc_gdp_corr)


# Correlation between the long_term interest rates and real house prices
prices_int_corr <- ggplot(final_df, aes(x = long_int_rates, y = real_house_prices)) +
  geom_point(color = "red4", alpha=0.6) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation is -0.3651, R^2 = 0.1333",
       x = "Long-term interest rates (percent per annum)",
       y = "real house prices (index)") +
  theme_minimal()

print(prices_int_corr)


# Correlation between the long_term interest rates and price to income ratio
price_inc_int_corr <- ggplot(final_df, aes(x = long_int_rates, y = price_to_income)) +
  geom_point(color = "purple4", alpha=0.6) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation is -0.1990, R^2 = 0.0396",
       x = "Long-term interest rates (percent per annum)",
       y = "House price to income ratio (index)") +
  theme_minimal()

print(price_inc_int_corr)



