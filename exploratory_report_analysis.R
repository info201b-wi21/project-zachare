# Section 1:

# Section 2: Heather

malnourishment <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Malnutrition%20Effects/country-wise-average.csv?token=ASLWM4NANTQ3W277GORWSZTAHWFMO")
health_expenditures <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWM4KWCSH5B33LJMJPOTTAHWFRW")

library("dplyr")
library("tidyverse")
library("ggplot2")

malnourishment_sample <- malnourishment %>%
  top_n(5) %>%
  select(Country, Wasting, Overweight, Underweight)

health_expenditures <- health_expenditures %>%
  rename(
    "1990" = X1990..YR1990.,
    "2000" = X2000..YR2000.,
    "2011" = X2011..YR2011.,
    "2012" = X2012..YR2012.,
    "2013" = X2013..YR2013.,
    "2014" = X2014..YR2014.,
    "2015" = X2015..YR2015.,
    "2016" = X2016..YR2016.,
    "2017" = X2017..YR2017.,
    "2018" = X2018..YR2018.,
    "2019" = X2019..YR2019.,
    "2020" = X2020..YR2020.
  )

health_expenditures_sample <- health_expenditures %>%
  select(Country.Name, "2017") %>%
  slice(1:3)

# Section 2.2: Zach

# Section 2.3: Liza

# Section 3: Heather

most_recent_year <- health_expenditures$"2018"

malnourishment <- na.omit(malnourishment) %>%
  mutate(
    total_malnourishment = Severe.Wasting + Wasting + Overweight + Stunting + Underweight
  )
 
health_expenditures <- health_expenditures %>%
  rename(
    "Country" = Country.Name
  )

health_expenditures$Country <-  toupper(health_expenditures$Country)

correlation_plot_df <- left_join(malnourishment, health_expenditures, by = c("Country")) %>%
  select(Country, total_malnourishment, "2018")

correlation_plot_df <- correlation_plot_df %>%
  rename(
    "health_expenditure_2018" = "2018"
  )

correlation_plot <- ggplot(correlation_plot_df, aes(x = health_expenditure_2018, y = total_malnourishment)) + 
  geom_point() +
  ggtitle("Scatterplot of health expenditures and malnourishment rates") +
  xlab("Health Expenditure (Current USD)") + 
  ylab("Malnourishment Rate") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())
  
# Section 3: Zach

# Section 3: Liza
