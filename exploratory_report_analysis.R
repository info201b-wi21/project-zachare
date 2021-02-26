# Section 2

malnourishment_df <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Malnutrition%20Effects/country-wise-average.csv?token=ASLWM4NANTQ3W277GORWSZTAHWFMO")
health_expenditures_df <- read.csv("https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/data/Health%20Expenditures/ae2e0043-4b54-4d83-af00-191617f8402b_Data.csv?token=ASLWM4KWCSH5B33LJMJPOTTAHWFRW")

library('knitr')
library('ggplot2')
library('scales')
library('maps')
library('RColorBrewer')
library('countrycode', exclude = "select")
library('dplyr')
library('tidyverse')
library('gapminder')

malnourishment_sample <- malnourishment %>%
  top_n(5) %>%
  select(Country, Wasting, Overweight, Underweight)

health_expenditures_df <- health_expenditures_df %>%
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

# Section 2.2 - Malnutrition Data Frame ########################

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

# Section 2.2 - Health Expenditure Data Frame

# convert the character data into numeric
health_expenditures_df <- health_expenditures_df %>% 
  mutate_at(.vars=c(6:16),.funs=funs(as.numeric(.)), na.rm = TRUE)

# get rid of rows that aren't individual countries (e.g., north america, arab world) 
health_expenditures_2_df <- health_expenditures_df %>% slice(1:217)

# calculate the average expenditure of the world in 2000, 2011, 2018
world_averages <- summarize(health_expenditures_2_df, 
  average_2000 = mean(year_2000, na.rm = TRUE), 
  average_2011 = mean(year_2011, na.rm = TRUE),
  average_2018 = mean(year_2018, na.rm = TRUE))

# calculate the maximum expenditure of any country in 2000, 2011, and 2018
world_max <- summarize(health_expenditures_df, 
  max_2000 = max(year_2000, na.rm = TRUE), 
  max_2011 = max(year_2011, na.rm = TRUE),
  max_2018 = max(year_2018, na.rm = TRUE))

# calculate the minimum expenditure of any country in 2000, 2011, and 2018
world_min <- summarize(health_expenditures_df, 
  min_2000 = min(year_2000, na.rm = TRUE), 
  min_2011 = min(year_2011, na.rm = TRUE), 
  min_2018 = min(year_2018, na.rm = TRUE))

########################

# calculate the rate change between 2000 and 2018 for each country
health_expenditures_rate_df <- health_expenditures_df %>% 
  mutate(expenditure_rate_change = (year_2018 - year_2000) / year_2018) %>%
  select(Country.Name, expenditure_rate_change, Country.Code)

# join the world data with the mutated data frame
worldData <- map_data('world') %>%
  mutate(Country.Code = countrycode(region, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(Country.Code = case_when(region == 'Kosovo' ~ 'XKX', TRUE ~ Country.Code)) %>%
  inner_join(health_expenditures_rate_df, by="Country.Code")


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),      
    axis.text = element_blank(),        
    axis.ticks = element_blank(),      
    axis.title = element_blank(),       
    panel.background = element_rect(fill = 'lightgrey'),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()     
  )


expenditure_rate_change_plot <- ggplot(worldData) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = expenditure_rate_change),
               color = "gray", size = .05) + coord_map(xlim=c(-180,180)) +
  scale_fill_gradientn(colours = rev(brewer.pal(9, name="YlGn")), labels = percent) + 
  labs(title="Expenditure (per capita) Rate Change from 2000 to 2018", caption = "", fill = "Change %") + 
  blank_theme

plot(expenditure_rate_change_plot)

# the two outlier countries here are Venezuela and democratic republic of Congo. 

##########################

# obtain the countries with the top 10 health expenditures in 2018
health_expenditures_top_10 <- health_expenditures_2_df %>% select(Country.Name, year_2018) %>% 
      arrange(desc(year_2018)) %>% head(10)

# save the top 10 countries in a vector to preserve the order
ordered_list_of_countries <- health_expenditures_top_10 %>% pull(Country.Name)

top_10_expenditures_plot <- ggplot(data=health_expenditures_top_10, 
  aes(reorder(ordered_list_of_countries, year_2018, sum), year_2018)) + coord_flip() +
  geom_bar(stat="identity", position=position_dodge(-0.9), fill = "#144f15") +
  labs(title="Countries With Highest Health Expenditures (per capita) in 2018",
       x ="Country", y = "Health Expenditure (per capita) in US $", fill = "") +
  scale_fill_discrete(guide = "none")


plot(top_10_expenditures_plot)

###########################

# simplify the data to only what will be used in the plot
average_trend_df <- health_expenditures_df %>%
  select(Country.Name, (7:14)) %>% 
  filter(Country.Name %in% c("World", "Russia", "United States", "China", "India", "Japan", 
                             "Germany", "Israel", "Brazil"))

pivoted_average_trend_df <- average_trend_df %>%
  pivot_longer(cols = (year_2011:year_2018), names_to = "years", ".value") %>%
  arrange(years)
  
years <- c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

labels <- c("World", "Russia", "United States", "China", "India", "Japan", 
            "Germany", "Israel", "Brazil")
average_trend_plot <- 
  ggplot(data=pivoted_average_trend_df, aes(x=years, y=value, group=Country.Name)) +
  geom_line(aes(color=Country.Name)) +
  geom_point(aes(color=Country.Name)) +
  scale_colour_brewer(labels=labels, palette = "Dark2") +
  labs(title="Average Health Expenditure Over Time",
       x ="Year", y = "Health Expenditure (per capita) US $", color = "Country") 

plot(average_trend_plot)

##################################








