# loading datasets

malnourishment_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/malnutrition-estimates%20(1).csv?token=ASLWDXAQUFCKJYCV6R5QMBDAJUQ7Y')

all_health_expenditures_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWDXHHC5OK6PNVNUEKEF3AJURCU')

# renaming columns in health expenditures dataframe

names(all_health_expenditures_df) <- gsub("^X[0-9]{4}..YR([0-9]{4})\\.$","amount_\\1", names(all_health_expenditures_df))
year_regex <- "^(amount)_([0-9]{4})$"

all_health_expenditures_df <- all_health_expenditures_df %>% mutate(across(matches(year_regex), as.numeric))

# get rid of rows that aren't individual countries (e.g., north america, arab world)
health_expenditures_df <- all_health_expenditures_df %>% slice(1:217)

# wrangling data for health expenditures plot 

countries = c("World", "Russian Federation", "United Kingdom", "Germany", "France", "Japan", "Israel", "United States", "China", "India")
country_trends_df <- all_health_expenditures_df %>%
  pivot_longer(cols = matches(year_regex),
               names_pattern = year_regex,
               names_to = c(".value", "year")
  ) %>%
  select(Country.Name, Country.Code, year, amount) %>%
  mutate(year = as.numeric(year))

start_year = 2011
end_year = 2018
