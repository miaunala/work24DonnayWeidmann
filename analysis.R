# Linear regression w/ fixed effects for country and year
model1 <- feols(prop_global ~ `Regional autonomy` + `Separatism/irredentism` | country_gwid + year, data = joined_data)
model1
