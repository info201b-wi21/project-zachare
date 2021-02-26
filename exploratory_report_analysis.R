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
  slice(1:924)

health_expenditures_df_graph <- health_expenditures_df_graph %>% 
  replace_with_na_all(condition = ~.x == "..") 

health_expenditures_graph <- ggplot(health_expenditures_df_graph, aes(x = factor(year_2018))) +
  geom_bar(stat = "count", fill = "red") +
  labs(title = "Distribution of Health Expenditures",
       x = "Health Expenditure",
       y = "Number of Countries") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
