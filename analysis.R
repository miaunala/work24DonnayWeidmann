# Fixed effects model for countries and years with clustered standard errors
library("lfe")
model1 <- felm(prop_global ~ `Regional autonomy` + `Separatism/irredentism` | country_gwid + year|0|country_gwid + year, data = joined_data)
summary(model1)

# Descriptive Analysis of global posts over years
library("ggplot2")
global_year <- ggplot(data = joined_data, mapping = aes(x = year, y = post_count_global)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Global Posts by Year",
       x = "Year",
       y = "Number of Global Posts") +
  theme_minimal()
global_year
