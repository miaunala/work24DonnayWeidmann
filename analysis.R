library("lfe")
library("ggplot2")
library("countrycode")

# Fixed effects model for countries and years with clustered standard errors
model1 <- felm(prop_global ~ `Regional autonomy` + `Separatism/irredentism` | country_gwid + year|0|country_gwid + year, data = joined_data)
summary(model1)

# Descriptive Analysis of global posts over years
global_year <- ggplot(data = joined_data, mapping = aes(x = year, y = post_count_global)) +
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

global_year_region <- ggplot(data = joined_data, mapping = aes(x = year, y = post_count_global, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Number of Global Posts by Year and Region",
       x = "Year",
       y = "Number of Global Posts") +
  scale_x_discrete(drop = FALSE) +  
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
global_year_region



