# Section 2

malnourishment <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Malnutrition%20Effects/country-wise-average.csv?token=ASLWM4NANTQ3W277GORWSZTAHWFMO")
health_expenditures <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWM4KWCSH5B33LJMJPOTTAHWFRW")

library("dplyr")
library("tidyverse")

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
  select(Country.Name, X2017..YR2017., X2018..YR2018.) %>%
  slice(1:3)
