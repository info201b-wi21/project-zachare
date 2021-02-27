

# Section 3: Heather

malnourishment_df_graph <- malnourishment_df %>%
  distinct(Country, .keep_all = T) 

income_classification_graph <- ggplot(malnourishment_df_graph, aes(x = factor(Income.Classification))) +
  geom_bar(stat = "count", fill = "steelblue") +
  labs(title = "Number of Countries in each Income Classification",
       x = "Income Classification",
       y = "Number of Countries") 

health_expenditures_df_graph <- health_expenditures %>%
  distinct(Country.Name, .keep_all = T) 

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
  geom_bar(fill = "red", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Health Expenditures",
       x = "Health Expenditure",
       y = "Number of Countries") +
  theme(text = element_text(size = 6))

# Section 3: Zach

overweight_average <- malnourishment_df %>%
  na.omit() %>%
  select(ISO.code, Overweight, U5.Population...000s.) %>%
  group_by(ISO.code) %>%
  summarize(avg_overweight = mean(Overweight), u5_pop = mean(U5.Population...000s.))


health_expenditure_avg <- health_expenditures_df %>%
  replace(is.na(.), 0) %>%
  group_by(Country.Code) %>%
  mutate(avg_expenditures = mean(amount_2011:amount_2018), na.rm = TRUE) %>%
  select(Country.Code, avg_expenditures) %>%
  filter(avg_expenditures > 0) %>%
  dplyr::rename("ISO.code" = "Country.Code") %>%
  left_join(., overweight_average, by = "ISO.code") %>%
  na.omit()

overweight_expenditure_plot <- ggplot(health_expenditure_avg, aes(x=avg_expenditures, y=avg_overweight, size=u5_pop)) +
  geom_point(alpha=0.5, color= "darkblue", fill = NA) +
  xlim(0, 2500) +
  scale_size(range = c(.1, 24), name="Population Under 5 (by thousands)") +
  labs(title = "Correlation Between Average Health Expenditures and Overweight Rates By Country", x = "Average Health Expenditures per Capita (in USD)", y = "Average Percentage of Overweightness (2011-2018)")
         
overweight_cor_test <- cor.test(health_expenditure_avg$avg_expenditures, health_expenditure_avg$avg_overweight, 
                method = "pearson")
overweight_p_val <- res$p.value  
