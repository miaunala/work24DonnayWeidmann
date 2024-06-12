library("lfe")
model1 <- felm(prop_global ~ `Regional autonomy` + `Separatism/irredentism` | country_gwid + year|0|country_gwid + year, data = joined_data)
summary(model1)
