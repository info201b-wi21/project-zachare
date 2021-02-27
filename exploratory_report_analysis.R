####### SECTION 3 ######################

malnourishment_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/malnutrition-estimates%20(1).csv?token=ASLWDXDCP5ZFM5WZEXBKM5LAIHHAE')
all_health_expenditures_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWDXEFNW4YWYFQVXMAME3AHWH2U')

# Zach's part

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

overweight_expenditure_plot <- ggplot(health_expenditure_avg, 
  aes(x=avg_expenditures, y=avg_overweight, size=u5_pop)) +
  geom_point(alpha=0.5, color = "steelblue") +
  scale_size(range = c(.1, 24), name="Population Under 5 (by thousands)", labels = comma) +
  labs(title = "Correlation Between Average Health Expenditures and Overweight Rates By Country", 
       x = "Average Health Expenditures per Capita (in USD)", 
       y = "Average Percentage of Overweightness (2011-2018)") +
  scale_x_continuous(labels = comma, limits=c(0,2500)) 

overweight_cor_test <- cor.test(health_expenditure_avg$avg_expenditures, health_expenditure_avg$avg_overweight, 
                                method = "pearson")
overweight_p_val <- overweight_cor_test$p.value 


####################################

# Heather's part

malnourishment_df_graph <- malnourishment_df %>%
  distinct(Country, .keep_all = T) 

income_classification_graph <- ggplot(malnourishment_df_graph, aes(x = factor(Income.Classification))) +
  geom_bar(stat = "count", fill = "steelblue") +
  labs(title = "Number of Countries in each Income Classification",
       x = "Income Classification",
       y = "Number of Countries") 

health_expenditures_df_graph <- health_expenditures_df %>%
  distinct(Country.Name, .keep_all = TRUE) 

breaks <- c(0, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000)

tags <- c("0-10", "10-20", "20-50", "50-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", 
          "5000-10000", "10000-20000") 

group_tags <-
  cut(health_expenditures_df_graph$amount_2018, na.rm = TRUE,
      breaks = breaks, include.lowest = TRUE, 
      right = FALSE, labels = tags) %>% na.omit()

summary(group_tags)

health_expenditures_df_graph <- health_expenditures_df_graph %>% 
  replace_with_na_all(condition = ~.x == "..") 

health_expenditures_graph <- ggplot(data = as_tibble(group_tags), 
                                    mapping = aes(x = value)) +
  geom_bar(fill = "steelblue", color = "white",alpha = 0.7) +
  labs(title = "Distribution of Health Expenditures (per capita)",
       x = "Health Expenditure in USD (per capita)",
       y = "Number of Countries") +
  theme(text = element_text(size = 7))


###########################################

# Liza's part

malnourishment_sample <- malnourishment_df %>%
  top_n(5) %>%
  select(Country, Year, Wasting, Overweight, Underweight, Stunting) %>%
  mutate(Country = str_to_title(Country))

start_year = 2011
end_year = 2018

categories = c("Wasting", "Overweight", "Underweight", "Stunting")
health_expenditures_average_rate_df <- health_expenditures_df %>%
  pivot_longer(cols = matches(year_regex), 
               names_pattern = year_regex,
               names_to = c(".value", "year")
  ) %>% 
  select(Country.Name, Country.Code, year, amount) %>%
  rename(average_health_expenditure = amount) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% (start_year:end_year)) %>%
  group_by(Country.Code, Country.Name) %>%
  summarize(average_health_expenditure = mean(average_health_expenditure, na.rm = TRUE), .groups="drop")

malnourishment_average_df <- malnourishment_df %>%
  rename(Country.Code = ISO.code) %>%
  filter(Year %in% (start_year:end_year)) %>%
  group_by(Country.Code) %>%
  summarize(Wasting = mean(Wasting, na.rm = TRUE),
            Overweight = mean(Overweight, na.rm = TRUE),
            Underweight = mean(Underweight, na.rm = TRUE),
            Stunting = mean(Stunting, na.rm = TRUE),
            Income.Classification = mean(Income.Classification, na.rm = TRUE),
            U5.Population...000s. = mean(U5.Population...000s., na.rm = TRUE))

pivot_malnourishment_average_df <- malnourishment_average_df %>%
  pivot_longer(cols = all_of(categories),
               names_to = "category",
               values_to = "rate") 

correlation_plot_df <- health_expenditures_average_rate_df %>%
  left_join(pivot_malnourishment_average_df, by = c("Country.Code")) %>%
  select(Country.Name, category, rate, average_health_expenditure, Income.Classification) %>%
  mutate(category = factor(category, levels=categories)) %>%
  na.omit()

correlation_plot <- ggplot(correlation_plot_df,
                           aes(x = average_health_expenditure, y = rate, group = category)) +
  geom_point(aes(color=category), alpha = 0.4) +
  # use quantile regression to fit and plot the "median" curve
  geom_quantile(aes(color=category), method = "rqss", lambda = 0.3, quantiles = 0.5, size=0.75) + 
  scale_colour_brewer(labels=categories, palette = "Dark2") +
  labs(title="Correlation of Countries' Health Expenditures and Malnourishment Rates",
       x ="Health Expenditure in USD (per capita)", y = "Malnourishment Rates in %", color = "Malnourishment\nCategory") +
  scale_x_continuous(labels = comma, trans = "log10") +
  scale_y_continuous(limits = c(0.0, 60.0))
