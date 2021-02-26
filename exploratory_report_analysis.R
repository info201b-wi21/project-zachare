# Section 3

most_recent_year <- health_expenditures %>%
  filter(year == 2018) %>%
  
malnourishment <- malnourishment %>%
  mutate(
    malnourishment = sum(Severe.Wasting, Wasting, Overweight, Stunting, Underweight)
  )
