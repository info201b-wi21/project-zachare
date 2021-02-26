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

# Section 2.2

income_class_mean <- mean(malnourishment$Income.Classification)

mean_u5_pop <- mean(malnourishment$U5.Population...000s.)

mean_total_malnourish <- mean(malnourishment$total_malnourishment)

max_total_malnourish <- max(malnourishment$total_malnourishment)

max_u5_pop <- max(malnourishment$U5.Population...000s.)

avg_sev_waste <- mean(malnourishment$Severe.Wasting)

avg_waste <- mean(malnourishment$Wasting)

avg_overweight <- mean(malnourishment$Overweight)

avg_stunting <- mean(malnourishment$Stunting)

avg_underweight <- mean(malnourishment$Underweight)

avg_mal_effects <- c(avg_overweight, avg_sev_waste, avg_stunting, avg_underweight, avg_waste)

mal_effects_names <- c("Overweight", "Severe Wasting", "Stunting", "Underweight", "Wasting")

mal_df <- data.frame(mal_effects_names, avg_mal_effects)


malnourishment_data_desc <- list(income_class_mean, mean_u5_pop, max_u5_pop, mean_total_malnourish, max_total_malnourish)

malnourishment_dens <- malnourishment %>% 
  ggplot(aes(x = total_malnourishment)) + geom_density(fill="#69b3a2", color = "#0000FF", alpha = 0.8) + labs(x = "Total malnourishment effects", y= "Density", title = "Density Graph of Total Malnourishment Effects")

mal_effects_pie <- ggplot(mal_df, aes(x = "", y = avg_mal_effects, fill = mal_effects_names)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void() + labs(title = "Effects of Malnutrition", fill = "Effects")

mal_bubble <- ggplot(malnourishment, aes(x=Income.Classification, y= total_malnourishment, size = U5.Population...000s.)) +
  geom_point(alpha=0.7) + labs(title = "Malnourishment by Income and Population", y = "Total Malnourishment", x = "Income Classification (0 = lowest, 3 = highest)", size = "Population Under 5 (by thousands)") 

