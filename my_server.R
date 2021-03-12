malnourishment_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/malnutrition-estimates%20(1).csv?token=ASLWDXAQUFCKJYCV6R5QMBDAJUQ7Y')
all_health_expenditures_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWDXHHC5OK6PNVNUEKEF3AJURCU')
names(all_health_expenditures_df) <- gsub("^X[0-9]{4}..YR([0-9]{4})\\.$","amount_\\1", names(all_health_expenditures_df))
year_regex <- "^(amount)_([0-9]{4})$"
all_health_expenditures_df <- all_health_expenditures_df %>% mutate(across(matches(year_regex), as.numeric))

health_expenditures_group_df <- all_health_expenditures_df %>% 
  slice(1:217) %>%
  pivot_longer(cols = matches(year_regex), 
               names_pattern = year_regex,
               names_to = c(".value", "year")) %>% 
  select(Country.Name, Country.Code, year, amount) %>%
  mutate(continent = countrycode(Country.Name, origin = "country.name", destination = "continent")) %>%
  group_by(Country.Code, Country.Name, continent)

malnourishment_group_df <- malnourishment_df %>%
  rename(Country.Code = ISO.code) %>%
  group_by(Country.Code)

source("regression_line_plot.R")
source("line_plot.R")
source("bubble_plot.R")
source("choropleth_map.R")

server <- function(input,output) {
  output$africa_choropleth_map <- renderPlotly({
    make_choropleth_map(input)
  })
  
  output$correlation_regression_line_plot <- renderPlotly({
    make_regression_line_plot(input)
  })
  
  output$country_line_plot <- renderPlotly({
    make_line_plot(input)
  })
  
  output$overweight_bubble_plot <- renderPlotly({
    make_bubble_plot(input)
  })

}

