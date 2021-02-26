# Section 3: Heather

malnourishment_df_graph <- malnourishment_df %>%
  distinct(Country, .keep_all = T)

income_classification_graph <- ggplot(malnourishment_df_graph, aes(x = factor(Income.Classification))) +
  geom_bar(stat = "count", fill = "steelblue") +
  labs(title = "Number of Countries in each Income Classification",
       x = "Income Classification",
       y = "Number of Countries") 
