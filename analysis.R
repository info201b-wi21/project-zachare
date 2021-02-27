library('knitr')
library('ggplot2')
library('scales')
library('maps')
library('RColorBrewer')
library('countrycode', exclude = "select")
library('dplyr')
library('tidyverse')
library('naniar')

malnourishment_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/malnutrition-estimates%20(1).csv?token=ASLWDXDCP5ZFM5WZEXBKM5LAIHHAE')
all_health_expenditures_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWDXEFNW4YWYFQVXMAME3AHWH2U')
names(all_health_expenditures_df) <- gsub("^X[0-9]{4}..YR([0-9]{4})\\.$","amount_\\1", names(all_health_expenditures_df))
year_regex = "^(amount)_([0-9]{4})$"
all_health_expenditures_df <- all_health_expenditures_df %>%
  mutate(across(matches(year_regex), as.numeric))

all_health_expenditures_sample_df <- all_health_expenditures_df %>%
  slice(1:3) %>%
  select(Country.Name, amount_2017, amount_2018)

most_recent_year_health_expenditures <- all_health_expenditures_df$amount_2018

########## SECTION 2.2 - HEALTH EXPENDITURES ###########

# get rid of rows that aren't individual countries (e.g., north america, arab world)
health_expenditures_df <- all_health_expenditures_df %>% slice(1:217)

# calculate the average expenditure of the world in 2000, 2011, 2018
summary_statistics_df <- summarize(all_health_expenditures_df,
                            mean_2000 = mean(amount_2000, na.rm = TRUE),
                            mean_2011 = mean(amount_2011, na.rm = TRUE),
                            mean_2018 = mean(amount_2018, na.rm = TRUE),
                            max_2000 = max(amount_2000, na.rm = TRUE),
                            max_2011 = max(amount_2011, na.rm = TRUE),
                            max_2018 = max(amount_2018, na.rm = TRUE),
                            min_2000 = min(amount_2000, na.rm = TRUE),
                            min_2011 = min(amount_2011, na.rm = TRUE),
                            min_2018 = min(amount_2018, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_pattern = "^(max|mean|min)_([0-9]{4})$",
               names_to = c(".value", "year"))


#######################################

health_expenditures_rate_df <- health_expenditures_df %>%
  mutate(expenditure_rate_change = (amount_2018 - amount_2000) / amount_2000) %>%
  select(Country.Name, Country.Code, expenditure_rate_change)

worldData <- map_data('world') %>%
  mutate(Country.Code = countrycode(region, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(Country.Code = case_when(region == 'Kosovo' ~ 'XKX',
                                  TRUE ~ Country.Code)) %>%
  inner_join(health_expenditures_rate_df, by="Country.Code")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = 'darkgrey'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

expenditure_rate_change_plot <- ggplot(worldData) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill=expenditure_rate_change),
               color = "grey", size = .05) + coord_map(xlim=c(-180, 180)) +
  scale_fill_gradientn(colours = brewer.pal(9, name="YlGn"), labels = percent) +
  labs(title="Health Expenditure in USD (per capita) change from 2000 to 2018", caption = "", fill = "Change %") +
  blank_theme

plot(expenditure_rate_change_plot)

########################################

health_expenditures_top_10 <- health_expenditures_df %>%
  select(Country.Name, amount_2018) %>%
  arrange(desc(amount_2018)) %>%
  mutate(Country.Name = factor(Country.Name, levels=rev(Country.Name))) %>%
  head(10)

top_10_expenditures_plot <- ggplot(data=health_expenditures_top_10,
                                   aes(x=amount_2018, y=Country.Name)) +
  geom_bar(stat="identity", fill="steelblue", position=position_dodge(-0.9)) +
  geom_text(aes(label=comma(round(amount_2018, digits = 0))), hjust=1.2, color="white", size=3.5) +
  scale_x_continuous(labels = comma) +
  labs(title="Countries With Highest Health Expenditures in 2018",
       y ="Country", x = "Health Expenditure in USD (per capita)", fill = "")

plot(top_10_expenditures_plot)

##################################################

countries = c("World", "Russian Federation", "United Kingdom", "Germany", "France", "Japan", "Israel", "United States", "China", "India")
country_trends_df <- all_health_expenditures_df %>%
  pivot_longer(cols = matches(year_regex),
               names_pattern = year_regex,
               names_to = c(".value", "year")
  ) %>%
  select(Country.Name, Country.Code, year, amount) %>%
  mutate(year = as.numeric(year))

start_year = 2011
end_year = 2018
country_trend_plot <-
  ggplot(data=country_trends_df %>%
           filter(Country.Name %in% countries) %>%
           mutate(Country.Name = factor(Country.Name, levels=countries)),
         aes(x=year, y=amount, group=Country.Name)) +
  geom_line(aes(color=Country.Name)) +
  geom_point(aes(color=Country.Name)) +
  scale_colour_brewer(labels=countries, palette = "Paired") +
  labs(title=sprintf("Health Expenditure in USD (per capita) from %d to %d", start_year, end_year),
       x ="Year", y = "Health Expenditure in USD (per capita)", color = "Country") +
  scale_x_continuous(limits = c(start_year, end_year)) +
  scale_y_continuous(label=comma)

plot(country_trend_plot)



######### SECTION 2.2 - MALNOURISHMENT #########

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

income_class_mean <- mean(malnourishment_average_df$Income.Classification, na.rm=TRUE)
mean_u5_pop <- mean(malnourishment_average_df$U5.Population...000s., na.rm=TRUE)
mean_wasting <- mean(malnourishment_average_df$Wasting, na.rm=TRUE)
mean_underweight <- mean(malnourishment_average_df$Underweight, na.rm=TRUE)
mean_overweight <- mean(malnourishment_average_df$Overweight, na.rm=TRUE)
mean_stunting <- mean(malnourishment_average_df$Stunting, na.rm=TRUE)

max_u5_pop <- max(malnourishment_average_df$U5.Population...000s., na.rm=TRUE)
max_wasting <- max(malnourishment_average_df$Wasting, na.rm=TRUE)
max_underweight <- max(malnourishment_average_df$Underweight, na.rm=TRUE)
max_overweight <- max(malnourishment_average_df$Overweight, na.rm=TRUE)
max_stunting <- max(malnourishment_average_df$Stunting, na.rm=TRUE)

avg_mal_effects <- c(mean_overweight, mean_stunting, mean_underweight, mean_wasting)

malnourishment_categories_names <- c("Overweight", "Stunting", "Underweight", "Wasting")

mal_df <- data.frame(malnourishment_categories_names, avg_mal_effects)

knit_mal_df <- mal_df %>%
  rename("Effects" = malnourishment_categories_names, "Average Percentage" = avg_mal_effects)

malnourishment_data_desc <- list(income_class_mean, mean_u5_pop, max_u5_pop,
                                 mean_overweight, max_overweight,
                                 mean_stunting, max_stunting,
                                 mean_underweight, max_underweight,
                                 mean_wasting, max_wasting)

overweight_density <- malnourishment_average_df %>%
  ggplot(aes(x = Overweight)) +
  geom_density(fill="#69b3a2", color = "#0000FF", alpha = 0.8) +
  labs(x = "Overweight rate in %", y = "Density", title = "Overweight Rate Distribution Density Plot")

malnourishment_categories_bar <- ggplot(mal_df, aes(x=(malnourishment_categories_names),
  y = (avg_mal_effects), fill = as.factor(malnourishment_categories_names)) ) +
  geom_bar(stat = "identity" ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") +
  labs(title = "Global Rates of Malnourishment Effects",
       x = "Malnourishment Effect", y = "Percentage Effected")

stunting_bubble <- ggplot(malnourishment_average_df, aes(x=Income.Classification, y = Stunting, size = U5.Population...000s.)) +
  geom_point(alpha=0.7, color = "steelblue") +
  labs(title = "Stunting Rate by Income and Population", y = "Stunting rate in %",
       x = "Income Classification (0 = lowest, 3 = highest)", size = "Population Under 5 (in thousands)") +
  scale_size_continuous(labels = comma)

plot(stunting_bubble)



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




