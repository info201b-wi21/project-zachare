library('knitr')
library('ggplot2')
library('scales')
library('maps')
library('RColorBrewer')
library('countrycode', exclude = "select")
library('dplyr')
library('tidyverse')

### Reading Health Expenditures dataset ###
all_health_expenditures_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWDXEFNW4YWYFQVXMAME3AHWH2U')
names(all_health_expenditures_df) <- gsub("^X[0-9]{4}..YR([0-9]{4})\\.$","amount_\\1", names(all_health_expenditures_df))
year_regex = "^(amount)_([0-9]{4})$"
all_health_expenditures_df <- all_health_expenditures_df %>%
  mutate(across(matches(year_regex), as.numeric))

all_health_expenditures_sample_df <- all_health_expenditures_df %>%
  top_n(5) %>%
  select(Country.Name, amount_2017, amount_2018)

most_recent_year_health_expenditures <- all_health_expenditures_df$amount_2018

### Summary stats for Health Expenditures ###

# get rid of rows that aren't individual countries (e.g., north america, arab world)
health_expenditures_df <- all_health_expenditures_df %>% slice(1:217)

# calculate the average of every country's expenditure in 2000 and in 2018
world_averages <- summarize(
  health_expenditures_df, average_2018 = mean(amount_2018, na.rm = TRUE),
  average_2018 = mean(amount_2018, na.rm = TRUE))

# calculate the maximum expenditure of any country in 2000 and 2018
world_max <- summarize(health_expenditures_df, max_2000 = max(amount_2000, na.rm = TRUE),
                       max_2018 = max(amount_2018, na.rm = TRUE))

### Health Expenditure change choropleth map ###

health_expenditures_rate_df <- health_expenditures_df %>%
  mutate(expenditure_rate_change = (amount_2018 - amount_2000) / amount_2018) %>%
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

### Bar Graph of 10 countries with highest health expenditures ###

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

### Health Expenditures trends for different countries ###

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

### Health Expenditure/Malnourishment correlation plot ###

malnourishment_df <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/malnutrition-estimates%20(1).csv?token=ASLWDXDCP5ZFM5WZEXBKM5LAIHHAE")
malnourishment_sample_df <- malnourishment_df %>%
  top_n(5) %>%
  select(Country, Year, Wasting, Overweight, Underweight, Stunting)

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
  select(Country.Name, category, rate, average_health_expenditure) %>%
  mutate(category = factor(category, levels=categories)) %>%
  na.omit()

correlation_plot <- ggplot(correlation_plot_df,
                           aes(x = average_health_expenditure, y = rate, group = category)) +
  geom_point(aes(color=category), alpha = 0.15) +
  geom_quantile(aes(color=category), method = "rqss", lambda = 0.3, quantiles = 0.5, size=0.75) + 
  scale_colour_brewer(labels=categories, palette = "Dark2") +
  labs(title="Correlation of Countries' Health Expenditures and Malnourishment Rates",
       x ="Health Expenditure in USD (per capita)", y = "Malnourishment Rates in %", color = "Malnourishment\nCategory") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(limits = c(0.0, 60.0))

correlation_plot

### Malnourishment summary stats and 2 graphs ###

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

mal_effects_names <- c("Overweight", "Stunting", "Underweight", "Wasting")

mal_df <- data.frame(mal_effects_names, avg_mal_effects)

malnourishment_data_desc <- list(income_class_mean, mean_u5_pop, max_u5_pop, 
                                 mean_overweight, max_overweight, 
                                 mean_stunting, max_stunting,
                                 mean_underweight, max_underweight,
                                 mean_wasting, max_wasting)

overweight_density <- malnourishment_average_df %>% 
  ggplot(aes(x = Overweight)) + 
  geom_density(fill="#69b3a2", color = "#0000FF", alpha = 0.8) + 
  labs(x = "Overweight rate in %", y = "Density", title = "Overweight Rate Distribution Density Plot")

mal_effects_bar <- ggplot(mal_df, aes(x=(mal_effects_names), y = (avg_mal_effects), fill = as.factor(mal_effects_names)) ) + 
  geom_bar(stat = "identity" ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") +
  labs(title = "Global Rates of Malnourishment Effects", x = "Malnourishment Effects", y = "Percentage Effected")

stunting_bubble <- ggplot(malnourishment_average_df, aes(x=Income.Classification, y = Stunting, size = U5.Population...000s.)) +
  geom_point(alpha=0.7, color = "darkgreen") + 
  labs(title = "Stunting Rate by Income and Population", y = "Stunting rate in %", 
       x = "Income Classification (0 = lowest, 3 = highest)", size = "Population Under 5 (in thousands)") + 
  scale_size_continuous(labels = comma)
