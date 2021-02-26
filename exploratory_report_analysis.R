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
