# Analysis file

# Setup (same for both versions)

library("lfe")
library("ggplot2")
library("countrycode")
library("readr")
library("dplyr")
library("fixest")

setwd("C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay/work24DonnayWeidmann/data")

joined_data <- read_csv("joined_data.csv")


# Add country name and region (for region and country FE later)

joined_data$country_name <- countrycode(joined_data$gwid.x, "gwn", "country.name")
joined_data$region <- countrycode(joined_data$gwid.x, "gwn", "region")


# V1 - Current version

# DV: global_max & country_max & global with Fixed effects & Clustered Errors
# Ideas for future: 
# global~length (ob eine gewisse Länge eher dafür spricht globale/nationale Themen anzusprechen-> braucht man aber Mediatoren)
# add fixed effects for regions


# Global max models: channel_type, language pred, multiethn flag

# Channel type & global_max
# doesn't work yet, as there is only one platform in the dataset: Does the platform influence how global the conversation is? 
model_global_max_channel_type <- felm(global_max ~ as.factor(channel_type) | gwid.x + year | 0 | gwid.x + year, data = joined_data)
summary(model_global_max_channel_type)

# Language prediction & global_max: How does the choice of language in the messages affect the globality of the discussions?
model_global_max_lang_pred <- felm(global_max ~ as.factor(language_pred)  | gwid.x + year | 0 | gwid.x + year, data = joined_data)
summary(model_global_max_lang_pred)
# The predicted language has a significant but moderate impact on global_max, with certain 
# languages strongly predicting higher global topic probabilities.

# Multiethnic flag & global_max: Are multiethnic organisations more inclined to have a conversation about global topics?
model_global_max_multethn_flag <- felm(global_max ~ multiethnic_flag| gwid.x + year | 0 | gwid.x + year, data = joined_data)
summary(model_global_max_multethn_flag)
# Messages from multiethnic organisations are significantly more likely to address 
# global topics, as indicated by the positive and significant coefficient of the multiethnic_flag.


# Country max models: language_pred, channel_type, elect_part, status_excl_epr_static (maybe later status_exclepr_year see comment below)

# Language prediction & country_max: How does the choice of language in the messages affect the nationwide scope of the discussions?
model_cntry_max_lang_pred <- felm(country_max ~ as.factor(language_pred) | gwid.x + year | 0 | gwid.x + year, data = joined_data)
summary(model_cntry_max_lang_pred)
# Similar results as above with global_max

# Channel type & country_max: Does the platform influence how national the conversation is? 
# doesn't work yet, as there is only one platform in the dataset: Does the platform influence how global the conversation is? 
model_cntry_max_channel_type <- felm(country_max ~ as.factor(channel_type) | gwid.x + year | 0 | gwid.x + year, data = joined_data)
summary(model_cntry_max_channel_type)

# Electoral participation & country_max: Does the electoral participation of an organisation lead to more discussions on a national level?
model_cntry_max_elect_part <- felm(country_max ~ elect_part | gwid.x + year | 0 | gwid.x + year, data = joined_data)
summary(model_cntry_max_elect_part)
# Not significant

# Status_excl_epr_static & country_max: Does the status (exclusion) influence how national a conversation of an organisation is?
model_cntry_max_status_excl_epr_static <- felm(country_max ~ status_excl_epr_static | gwid.x + year | 0 | gwid.x + year, data = joined_data)
summary(model_cntry_max_status_excl_epr_static)
# Not significant
# Maybe could do a model with the exclusion status per year as well (data needs to be grouped beforehand)


# Global models: channel_type, gwid.x, org_age, status_excl_epr_static, ed_language1 (maybe groupid_epr later on; status_exclepr_year see comment below)

# channel_type & global: 
# doesn't work yet, as there is only one platform in the dataset: Does the platform influence how global the conversation is? 
model_global_channel_type <- feglm(global ~ as.factor(channel_type) | gwid.x + year, family=binomial(link="logit"), data = joined_data)
summary(model_global_channel_type)

# gwid.x & global: Is the conversation within a country more focussed on national or global topics?
# Disclaimer: only with year fixed effects; potential collinearity issues 
model_global_gwidx <- feglm(global ~ as.factor(gwid.x) | year, family=binomial(link="logit"), data = joined_data)
summary(model_global_gwidx)
# Some countries show a significant effect on whether the conversation is global or national, albeit it is only a moderate effect.

# org_age & global: Do topics become more national or global with rising age of organisations?
model_global_org_age <- feglm(global ~ org_age | gwid.x +year, family=binomial(link="logit"), data = joined_data)
summary(model_global_org_age)
# The age of an organisation has a slightly significant and moderate negative effect on the globality of the discourse.

# status_excl_epr_static & global: Does the status (exclusion) influence whether the conversation is global or national?
model_global_status_excl_epr_static <- feglm(global ~ status_excl_epr_static | gwid.x +year, family=binomial(link="logit"), data = joined_data)
summary(model_global_status_excl_epr_static)
# Not significant

# ed_language1 & global: Does the language in education influence whether the conversation is global or national
model_global_ed_language1 <- feglm(global ~ ed_language1 | gwid.x +year, family=binomial(link="logit"), data = joined_data)
summary(model_global_ed_language1)
# The language in education has a significant impact on whether the conversation is rather global or national.
# Disclaimer: Collinearity issues detected



# Descriptives machen: 

# Descriptives of variables & correlations

# Global / Country Max plots

# Country

# Global Max by Country
plt_globalmax_cntry <- ggplot(joined_data, aes(x = as.factor(gwid.x), y = global_max)) +
  geom_boxplot() +
  labs(title = "Global Max by Country",
       x = "Country",
       y = "Global Max") +
  theme_minimal()
ggsave("Global_Max_Country.jpg", plot = plt_globalmax_cntry, bg="white")
# The probability of having global conversations varies widely between countries
# but for most countries the probability is relatively low.

# Country Max by Country
plt_cntrymax_cntry <- ggplot(joined_data, aes(x = as.factor(gwid.x), y = country_max)) +
  geom_boxplot() +
  labs(title = "Country Max by Country",
       x = "Country",
       y = "Country Max") +
  theme_minimal()
ggsave("Country_Max_Country.jpg", plot = plt_cntrymax_cntry, bg="white")
# The probability of having national conversations stays for the bigger part in
# most countries below a half.


# Channel Type

# Global Max by Channel Type
plt_globalmax_channel_type <- ggplot(joined_data, aes(x = as.factor(channel_type), y = global_max)) +
  geom_boxplot() +
  labs(title = "Global Max by Channel Type",
       x = "Channel Type",
       y = "Global Max") +
  theme_minimal()
ggsave("Global_Max_Channel_Type.jpg", plot = plt_globalmax_channel_type, bg="white")
# This plot is currently not really pertinent, as only one platform has been
# registered in the data.

# Country Max by Channel Type
plt_cntrymax_channel_type <- ggplot(joined_data, aes(x = as.factor(channel_type), y = country_max)) +
  geom_boxplot() +
  labs(title = "Country Max by Channel Type",
       x = "Channel Type",
       y = "Country Max") +
  theme_minimal()
ggsave("Country_Max_Channel_Type.jpg", plot = plt_cntrymax_channel_type, bg="white")
# This plot is currently not really pertinent, as only one platform has been
# registered in the data.


# Multiethnic Flag

# Global Max by Multiethnicity flag
plt_globalmax_multiethnicity <- ggplot(joined_data, aes(x = as.factor(multiethnic_flag), y = global_max)) +
  geom_boxplot() +
  labs(title = "Global Max by Multiethnicity",
       x = "Multiethnicity",
       y = "Global Max") +
  scale_x_discrete(labels=c("No", "Yes"))+
  theme_minimal()
ggsave("Global_Max_Multiethnicity.jpg", plot = plt_globalmax_multiethnicity, bg="white")
# If the organisation is multiethnic, the conversations are basically all global,
# whereas when the organisation isn't multiethnic, the conversation has a tendency
# to not be global but with many outliers.

# Country Max by Multiethnicity flag
plt_cntrymax_multiethnicity <- ggplot(joined_data, aes(x = as.factor(multiethnic_flag), y = country_max)) +
  geom_boxplot() +
  labs(title = "Country Max by Multiethnicity",
       x = "Multiethnicity",
       y = "Country Max") +
  scale_x_discrete(labels=c("No", "Yes"))+
  theme_minimal()
ggsave("Country_Max_Multiethnicity.jpg", plot = plt_cntrymax_multiethnicity, bg="white")
# Independent of the multiethnicity of organisations, the conversations have a 
# lower probability to be about national topics, albeit when the organisation
# is not multiethnic there are also many outliers that the conversation is indeed
# rather focussed on national issues.


# Organisation Age

# not happy with how these plot look yet
# Global Max by Organisation age
plt_globalmax_org_age <- ggplot(joined_data, aes(x = org_age, y = global_max)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Global Max by Organization Age",
       x = "Organization Age",
       y = "Global Max") +
  theme_minimal()
ggsave("Global_Max_Org_Age.jpg", plot = plt_globalmax_org_age, bg="white")
# In the plot is a slight tendency that with growing age posts become more likely
# to be global, however, the data points are too randomly distributed to make a
# general assumption.

# Country Max by Organisation age
plt_cntrymax_org_age <- ggplot(joined_data, aes(x = org_age, y = country_max)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Country Max by Organization Age",
       x = "Organization Age",
       y = "Country Max") +
  theme_minimal()
ggsave("Country_Max_Org_Age.jpg", plot = plt_cntrymax_org_age, bg="white")
# Same as above, only the tendency to be more nation-minded is slightly higher.

# Perhaps box plots for global and then group the organisation age by 10 years


# Static exclusion status

# Global Max by Static exclusion status
plt_globalmax_static_excl <- ggplot(na.omit(joined_data[, c("global_max", "status_excl_epr_static")]), aes(x = as.factor(status_excl_epr_static), y = global_max)) +
  geom_boxplot() +
  labs(title = "Global Max by Static Exclusion Status",
       x = "Static Exclusion Status",
       y = "Global Max") +
  scale_x_discrete(labels=c("No", "Yes"))+
  theme_minimal()
ggsave("Global_Max_static_exclusion.jpg", plot = plt_globalmax_static_excl, bg="white")
# The probability of having global posts is for both excluded and included organisations
# relatively low, albeit there are many outliers in both cases.

# Country Max by Static exclusion status
plt_cntrymax_static_excl <- ggplot(na.omit(joined_data[, c("global_max", "status_excl_epr_static")]), aes(x = as.factor(status_excl_epr_static), y = global_max)) +
  geom_boxplot() +
  labs(title = "Country Max by Static Exclusion Status",
       x = "Static Exclusion Status",
       y = "Country Max") +
  scale_x_discrete(labels=c("No", "Yes"))+
  theme_minimal()
ggsave("Country_Max_static_exclusion.jpg", plot = plt_cntrymax_static_excl, bg="white")
# Same result as above is applicable to the probability of having posts focussed
# on country issues. Thus, I would argue that the status of an organisation does 
# neither explain the likelihood of globality of posts nor the likelihood of 
# a nation-focus of posts.

# Exclusions status per year tbc

# global Yes/No plots tbc




# work update dokument updaten mit links zu den reports (diese auf google drive drauf)

# verschicken


















# V2 - Previous elaborate version
# Regressions

## Create time lags for potential later time series analysis

### Governmental power, Regional autonomy, Separatism/irridentism
joined_data <- joined_data %>%
  group_by(org_id) %>%
  mutate(gov_pow_lag = lag(gov_pow, order_by = year)) %>%
  mutate(reg_autonom_lag = lag(reg_autonom, order_by = year)) %>%
  mutate(sep_irred_lag = lag(sep_irred, order_by = year)) %>%
  ungroup()


## Fixed Effects Models

### Fixed effects model for countries and years with clustered standard errors
model1 <- felm(prop_global ~ as.factor(gov_pow) + as.factor(reg_autonom) + as.factor(sep_irred) | country_gwid + year|0|country_gwid + year, data = joined_data)
summary(model1)

### same but with control variable post_count
model2 <- felm(prop_global ~ as.factor(gov_pow) +as.factor(reg_autonom) + as.factor(sep_irred) + post_count | country_gwid + year|0|country_gwid + year, data = joined_data)
summary(model2)

###### post_count schauen ob model2 oder extended_model2 mehr sinn macht
## Log-transformed model
joined_data$log_post_count <- log(joined_data$post_count + 1)  # Adding 1 to avoid log(0)
### Extended regression model including log-transformed post count
extended_model2 <- felm(prop_global ~ as.factor(gov_pow) + as.factor(reg_autonom) + as.factor(sep_irred) + log_post_count | country_gwid + year | 0 | country_gwid + year, data = joined_data)
summary(extended_model2)



### Fixed effects model for regions and years with clustered standard errors
model_regions1 <- felm(prop_global ~ as.factor(gov_pow) + as.factor(reg_autonom) + as.factor(sep_irred) | region + year|0|region + year, data = joined_data)
summary(model_regions)

### Same but with control variable post_count 
model_regions2 <- felm(prop_global ~ as.factor(gov_pow) +as.factor(reg_autonom) + as.factor(sep_irred) +post_count | region + year|0|region + year, data = joined_data)
summary(model_regions2)


### wie erklärt man die anzahl der globalen Kommunikation von Gruppen? -> AIms einer Organisation, evtl Gruppenvariablen (evtl aber dann zu heterogene), 
## Ethnic claim 
### Removing NAs from specific column
df_modelclaim <- joined_data[!is.na(joined_data$eth_claim), ]
### Fixed Effects model for ethnic claim with countries and years and their clustered standard errors
model_claim <- felm(prop_global~eth_claim | country_gwid + year |0|country_gwid+year, data=df_modelclaim)

### same but with control variable
model_claim <- felm(prop_global~eth_claim + post_count | country_gwid + year |0|country_gwid+year, data=df_modelclaim)

### same but with region
model_claim_region <- felm(prop_global~eth_claim + post_count | region + year |0|region+year, data=df_modelclaim)






### ==> was der Typ von den orgsniationen ist (Rebellen etc.) ==> cannot answer this up till now

### => Fragen: wie power access die proportion of globa posts beeinflusst; Konflikte (ongoing violent conflict) proportion of global posts beeinflusst
  #### nachher: wenn problem mit conflict data geklärt ist

### other group vars



## descriptive / time analysis?
###Time trend -> global Themen over time? über alle Gruppen hinweg 
### conflict pro jahr & gruppe auf global posts




# Descriptive Analyses
## missing: time analysis: golbal themen over time über alle gruppen hinweg -> da oder?
### conflict pro jahr & gruppe
### prop global -> pro Organisations / Jahr


### Descriptive Analysis of global posts over years
global_year <- ggplot(data = joined_data, mapping = aes(x = year, y = global_posts)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Global Posts by Year",
       x = "Year",
       y = "Number of Global Posts") +
  theme_minimal()
global_year

### Descriptive Analysis of the proportion of global posts over years
prop_global_year <- ggplot(data = joined_data, mapping = aes(x = year, y = prop_global)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Global Posts by Year",
       x = "Year",
       y = "Proportion of Global Posts") +
  theme_minimal()
prop_global_year


### Descriptive Analysis of global posts over years and regions
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


### Descriptive Analysis of proportion of global posts over years and regions
propglobal_year_region <- ggplot(data = joined_data, mapping = aes(x = year, y = prop_global, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Proportion of Global Posts by Year and Region",
       x = "Year",
       y = "Proportion of Global Posts") +
  scale_x_discrete(drop = FALSE) +  
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
propglobal_year_region







# Welche Organisationen verwenden mehr globale Posts? Hier würde ich einfach nach “governmental power”, “regional autonomy” und “separatism” unterscheiden.
# Calculate the average share of global posts for each organisation according to the variables
summary_data <- joined_data %>%
  group_by(gov_pow, reg_autonom, sep_irred, orgname) %>%
  summarise(
    avg_prop_global = mean(prop_global, na.rm = TRUE),
    .groups = "drop"
  )
summary_data <- summary_data %>%
  filter(!is.na(gov_pow), !is.na(avg_prop_global))

# Boxplot für den Anteil globaler Posts nach "Governmental power"
ggplot(summary_data, aes(x = gov_pow, y = avg_prop_global)) +
  geom_boxplot() +
  labs(title = "Average Proportion of Global Posts by Governmental Power",
       x = "Governmental Power",
       y = "Average Proportion of Global Posts") +
  theme_minimal()

# Boxplot für den Anteil globaler Posts nach "Regional autonomy"
ggplot(summary_data, aes(x = reg_autonom, y = avg_prop_global)) +
  geom_boxplot() +
  labs(title = "Average Proportion of Global Posts by Regional Autonomy",
       x = "Regional Autonomy",
       y = "Average Proportion of Global Posts") +
  theme_minimal()

# Boxplot für den Anteil globaler Posts nach "Separatism/irredentism"
ggplot(summary_data, aes(x = sep_irred, y = avg_prop_global)) +
  geom_boxplot() +
  labs(title = "Average Proportion of Global Posts by Separatism/Irredentism",
       x = "Separatism/Irredentism",
       y = "Average Proportion of Global Posts") +
  theme_minimal()


# Mean over time 

# Group by year and Governmental power
gov_power_yearly <- joined_data %>%
  group_by(year, gov_pow) %>%
  summarise(
    avg_prop_global = mean(prop_global, na.rm = TRUE),
    .groups = "drop"
  )

# Group by year and Regional autonomy
reg_autonomy_yearly <- joined_data %>%
  group_by(year, reg_autonom) %>%
  summarise(
    avg_prop_global = mean(prop_global, na.rm = TRUE),
    .groups = "drop"
  )

# Group by year and Separatism/irredentism
sep_irredentism_yearly <- joined_data %>%
  group_by(year, sep_irred) %>%
  summarise(
    avg_prop_global = mean(prop_global, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(gov_power_yearly, aes(x = year, y = avg_prop_global, color = as.factor(gov_pow), group = gov_pow)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Proportion of Global Posts by Governmental Power Over Time",
       x = "Year",
       y = "Average Proportion of Global Posts",
       color = "Governmental Power") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(reg_autonomy_yearly, aes(x = year, y = avg_prop_global, color = as.factor(reg_autonom), group = reg_autonom)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Proportion of Global Posts by Regional Autonomy Over Time",
       x = "Year",
       y = "Average Proportion of Global Posts",
       color = "Regional Autonomy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(sep_irredentism_yearly, aes(x = year, y = avg_prop_global, color = as.factor(sep_irred), group = sep_irred)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Proportion of Global Posts by Separatism/Irredentism Over Time",
       x = "Year",
       y = "Average Proportion of Global Posts",
       color = "Separatism/Irredentism") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Global Yes / No plots
# Global plots

# Global by channel type
plt_channel_type_global <- ggplot(joined_data, aes(x = as.factor(channel_type), fill = as.factor(global))) +
  geom_bar(position = "dodge", color = "black", size = 0.5) +  
  labs(title = "Global Conversations by Channel Type",
       x = "Channel Type",
       y = "Count of Global Posts") +
  scale_fill_manual(values = c("white", "black"), labels = c("No", "Yes"), name = "Global") +
  theme_minimal()
ggsave("Global_by_Channel_Type.jpg", plot = plt_channel_type_global, bg = "white")


# Global by country
plt_gwid_global <- ggplot(joined_data, aes(x = as.factor(gwid.x), fill = as.factor(global))) +
  geom_bar(position = "dodge", color = "black", size = 0.5) +
  labs(title = "Global Conversations by Country",
       x = "Country",
       y = "Count of Global Posts") +
  scale_fill_manual(values = c("white", "black"), labels = c("No", "Yes"), name = "Global") +
  theme_minimal()
ggsave("Global_by_Country.jpg", plot = plt_gwid_global, bg = "white")


# Global by organisation age
# tbc


# Global by static exclusion status
plt_status_excl_global <- ggplot(na.omit(joined_data[, c("global", "status_excl_epr_static")]), aes(x = as.factor(status_excl_epr_static), fill = as.factor(global))) +
  geom_bar(position = "dodge", color = "black", size = 0.5) +
  labs(title = "Global Conversations by Static Exclusion Status",
       x = "Static Exclusion Status",
       y = "Count of Global Posts") +
  scale_fill_manual(values = c("white", "black"), labels = c("No", "Yes"), name = "Global") +
  theme_minimal()
ggsave("Global_by_Status_Exclusion.jpg", plot = plt_status_excl_global, bg = "white")


# Global by education language
plt_ed_language_global <- ggplot(joined_data, aes(x = as.factor(ed_language1), fill = as.factor(global))) +
  geom_bar(position = "dodge", color = "black", size = 0.5) +
  labs(title = "Global Conversations by Education Language",
       x = "Education Language",
       y = "Count of Global Posts") +
  scale_fill_manual(values = c("white", "black"), labels = c("No", "Yes"), name = "Global") +
  theme_minimal()
ggsave("Global_by_Education_Language.jpg", plot = plt_ed_language_global, bg = "white")






