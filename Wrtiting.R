# Function to count NAs for each column
count_na <- function(wage) {
  sapply(wage, function(col) sum(is.na(col)))
}

# Fix Age Grp (Population)
population <- population %>%
  mutate(
    age.group = case_when(
      age.group == "5-Sep" ~ "5-9",
      age.group == "Oct-14" ~ "10-14",
      TRUE ~ age.group
    )
  )

# Checking
count_na(birth)
count_na(wage)
count_na(employment)
count_na(epf_contribution)
count_na(population)
count_na(epf_savings)

# Umemployment Rate
ggplot(employment, aes(x=date, y=unemployment.rate....)) +
  geom_line(color = "darkgreen") +
  labs(title = "Unemployment Rate (1982–2023)", x = "Year", y = "Unemployment Rate (%)")

# Birth Rate 
ggplot(birth, aes(x = year, y = number.of.live.births.per.1000.population)) +
  geom_line(color = "firebrick") +
  labs(title = "Birth Rate Decline (2000–2023)", x = "Year", y = "Births per 1,000 Population")

# EPF Savings by Age Grp
epf_savings %>%
  filter(!As.at.31.December.2023 %in% c("<16",">85", "Grand Total","AGE GROUP (YEAR)")) %>%  #Excluding<16,>85,Grand Total,AGEGRP#
  ggplot(aes(x = As.at.31.December.2023, y =((as.numeric(X.5))/1e9), fill = As.at.31.December.2023)) +
  geom_bar(stat = "identity") +
  labs(title = "EPF Savings by Age Group (2023)", x = "Age Group", y = "Total Savings (RM Billion)")

# Median Wage by age Grp
ggplot(wage, aes(x = Year, y = (as.numeric(gsub(',','',Median.monthly.salaries...wages.of.employees..RM.)))/100,color=Age.group))+
  geom_line() +
  labs(title = "Median Wages by Age Group (2010–2022)", x = "Year", y = "Median Wage (RM'000)")

# Dependency Ratio Over Time
dependency_ratio <- population %>%
  mutate(
    age = case_when(
      age.group %in% c("0-4", "5-9", "10-14", "15-19") ~ "Youth",
      age.group %in% c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59") ~ "Working Age",
      TRUE ~ "Elderly"
    )
  ) %>%
  group_by(year, age) %>%
  summarise(population = sum(population)) %>%
  pivot_wider(names_from = age, values_from = population) %>%
  mutate(dependency_ratio = (Youth + Elderly) / `Working Age`)

ggplot(dependency_ratio, aes(x = year, y = dependency_ratio)) +
  geom_line(color = "steelblue") +
  labs(title = "Dependency Ratio Over Time", x = "Year", y = "Dependency Ratio")

# Wage Growth Heatmap by Age Grp 
wage_heatmap <- wage %>%
  filter(Sex == "Total") %>%
  mutate(Median.monthly.salaries...wages.of.employees..RM.=as.numeric(gsub(",","",
                                                                           Median.monthly.salaries...wages.of.employees..RM.))) %>%  #converting to numeric#
  group_by(Year,Age.group) %>%
  summarise(median_wage = median(Median.monthly.salaries...wages.of.employees..RM.))

ggplot(wage_heatmap, aes(x = Year, y = Age.group, fill = median_wage)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Median Wage (RM)") +  # Works for numeric data
  labs(title = "Wage Growth Heatmap by Age Group") +
  theme_minimal() 
