library("lfe")
library("ggplot2")
library("countrycode")

# Fixed effects model for countries and years with clustered standard errors
model1 <- felm(prop_global ~ `Regional autonomy` + `Separatism/irredentism` | country_gwid + year|0|country_gwid + year, data = joined_data)
summary(model1)

# Descriptive Analysis of global posts over years
global_year <- ggplot(data = joined_data, mapping = aes(x = year, y = global_posts)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Global Posts by Year",
       x = "Year",
       y = "Number of Global Posts") +
  theme_minimal()
global_year


# Add regions and countries
joined_data$country_name <- NA
joined_data$region <- NA

# Loop through each row and match the G&W code to the country name
for (i in 1:nrow(joined_data)) {
  gwid <- joined_data$country_gwid[i]
  print(gwid)
  joined_data$country_name[i] <- countrycode(gwid, "gwn", "country.name")
  joined_data$region[i] <- countrycode(gwid, "gwn", "region")
}

# Descriptive Analysis of global posts over years and regions
joined_data$year <- as.factor(joined_data$year)

global_year_region <- ggplot(data = joined_data, mapping = aes(x = year, y = global_posts, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Number of Global Posts by Year and Region",
       x = "Year",
       y = "Number of Global Posts") +
  scale_x_discrete(drop = FALSE) +  
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
global_year_region


# Welche Organisationen verwenden mehr globale Posts? Hier würde ich einfach nach “governmental power”, “regional autonomy” und “separatism” unterscheiden.
# Calculate the average share of global posts for each organisation according to the variables
summary_data <- joined_data %>%
  group_by(`Governmental power`, `Regional autonomy`, `Separatism/irredentism`, orgname) %>%
  summarise(
    avg_prop_global = mean(prop_global, na.rm = TRUE),
    .groups = "drop"
  )
summary_data <- summary_data %>%
  filter(!is.na(`Governmental power`), !is.na(avg_prop_global))

# Boxplot für den Anteil globaler Posts nach "Governmental power"
ggplot(summary_data, aes(x = `Governmental power`, y = avg_prop_global)) +
  geom_boxplot() +
  labs(title = "Average Proportion of Global Posts by Governmental Power",
       x = "Governmental Power",
       y = "Average Proportion of Global Posts") +
  theme_minimal()

# Boxplot für den Anteil globaler Posts nach "Regional autonomy"
ggplot(summary_data, aes(x = `Regional autonomy`, y = avg_prop_global)) +
  geom_boxplot() +
  labs(title = "Average Proportion of Global Posts by Regional Autonomy",
       x = "Regional Autonomy",
       y = "Average Proportion of Global Posts") +
  theme_minimal()

# Boxplot für den Anteil globaler Posts nach "Separatism/irredentism"
ggplot(summary_data, aes(x = `Separatism/irredentism`, y = avg_prop_global)) +
  geom_boxplot() +
  labs(title = "Average Proportion of Global Posts by Separatism/Irredentism",
       x = "Separatism/Irredentism",
       y = "Average Proportion of Global Posts") +
  theme_minimal()


# Mean over time 

# Group by year and Governmental power
gov_power_yearly <- joined_data %>%
  group_by(year, `Governmental power`) %>%
  summarise(
    avg_prop_global = mean(prop_global, na.rm = TRUE),
    .groups = "drop"
  )

# Group by year and Regional autonomy
reg_autonomy_yearly <- joined_data %>%
  group_by(year, `Regional autonomy`) %>%
  summarise(
    avg_prop_global = mean(prop_global, na.rm = TRUE),
    .groups = "drop"
  )

# Group by year and Separatism/irredentism
sep_irredentism_yearly <- joined_data %>%
  group_by(year, `Separatism/irredentism`) %>%
  summarise(
    avg_prop_global = mean(prop_global, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(gov_power_yearly, aes(x = year, y = avg_prop_global, color = as.factor(`Governmental power`), group = `Governmental power`)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Proportion of Global Posts by Governmental Power Over Time",
       x = "Year",
       y = "Average Proportion of Global Posts",
       color = "Governmental Power") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(reg_autonomy_yearly, aes(x = year, y = avg_prop_global, color = as.factor(`Regional autonomy`), group = `Regional autonomy`)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Proportion of Global Posts by Regional Autonomy Over Time",
       x = "Year",
       y = "Average Proportion of Global Posts",
       color = "Regional Autonomy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(sep_irredentism_yearly, aes(x = year, y = avg_prop_global, color = as.factor(`Separatism/irredentism`), group = `Separatism/irredentism`)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Proportion of Global Posts by Separatism/Irredentism Over Time",
       x = "Year",
       y = "Average Proportion of Global Posts",
       color = "Separatism/Irredentism") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






