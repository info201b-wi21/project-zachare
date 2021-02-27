# Section 2


malnourishment_df <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/malnutrition-estimates%20(1).csv?token=ASLWDXDCP5ZFM5WZEXBKM5LAIHHAE")
health_expenditures <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWM4KWCSH5B33LJMJPOTTAHWFRW")

library("dplyr")
library("tidyverse")
library("naniar")

malnourishment_sample <- malnourishment_df %>%
  top_n(5) %>%
  select(Country, Wasting, Overweight, Underweight)

health_expenditures <- health_expenditures %>%
  rename(
    "year_1990" = X1990..YR1990.,
    "year_2000" = X2000..YR2000.,
    "year_2011" = X2011..YR2011.,
    "year_2012" = X2012..YR2012.,
    "year_2013" = X2013..YR2013.,
    "year_2014" = X2014..YR2014.,
    "year_2015" = X2015..YR2015.,
    "year_2016" = X2016..YR2016.,
    "year_2017" = X2017..YR2017.,
    "year_2018" = X2018..YR2018.,
    "year_2019" = X2019..YR2019.,
    "year_2020" = X2020..YR2020.
  )

health_expenditures_sample <- health_expenditures %>%
  select(Country.Name, year_2017, year_2018) %>%
  slice(1:3)

# Section 3: Heather

malnourishment_df_graph <- malnourishment_df %>%
  distinct(Country, .keep_all = T) 

income_classification_graph <- ggplot(malnourishment_df_graph, aes(x = factor(Income.Classification))) +
  geom_bar(stat = "count", fill = "steelblue") +
  labs(title = "Number of Countries in each Income Classification",
       x = "Income Classification",
       y = "Number of Countries") 
  
malnourishment_df <- malnourishment_df %>%
  rename(Country.Code = ISO.code) 

health_expenditures_df_graph <- health_expenditures %>%
  left_join(malnourishment_df, by = c("Country.Code")) %>%
  arrange(Income.Classification) %>%
  distinct(Country.Name, .keep_all = T) %>%
  slice(1:152)

health_expenditures_df_graph$year_2018 <- as.numeric(as.character(health_expenditures_df_graph$year_2018, na.rm = T))

breaks <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 10000)

tags <- c("0-500", "500-1000", "1000-1500", "1500-2000", "2000-2500", "2500-3000", "3000-3500", "3500-4000", "4000-4500", "4500-5000", "5000-5500", "10000-10500")

group_tags <- cut(health_expenditures_df_graph$year_2018, na.rm = T,
                  breaks = breaks, 
                  include.lowest = TRUE, 
                  right = FALSE, 
                  labels = tags)

summary(group_tags)

health_expenditures_df_graph <- health_expenditures_df_graph %>% 
  replace_with_na_all(condition = ~.x == "..") 

health_expenditures_graph <- ggplot(data = as_tibble(group_tags), 
                                    mapping = aes(x = value)) +
  geom_bar(fill = "red", color = "white",alpha = 0.7) +
  labs(title = "Distribution of Health Expenditures",
       x = "Health Expenditure",
       y = "Number of Countries") +
  theme(text = element_text(size = 7))
       
