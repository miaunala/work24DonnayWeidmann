library(readr)
library(dplyr)
library(texreg)
library(fixest)
setwd("C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay")


# Read in data, create necessary variables, delete NAs and duplicates

# smdata
smdata <- read_csv("smdata.csv")
smdata$year <- as.numeric(substr(smdata$time, 7, 10))
smdata <- smdata %>% filter_at(vars(year, epr_groupid, country_gwid, accountname), all_vars(!is.na(.)))
smdata <- smdata %>% filter(!duplicated(select(., index_n)))


# eprdata
eprdata<- read_csv("eprdata.csv")
eprdata <- eprdata[!is.na(eprdata$orgname), ]
eprdata <- eprdata[!duplicated(eprdata), ]
# only keep needed variables
eprdata <- eprdata[, c("Governmental power", "Regional autonomy", "Separatism/irredentism", "org_id", "year", "gwid", "groupid")]

# eo2data
eo2data <- read_csv("eo2_with_latest_publication_date.csv")
eo2data <- eo2data[!is.na(eo2data$orgname), ]
eo2data <- eo2data %>% filter(!duplicated(select(., org_id, clean_username, channel_type)))
#eo2data <- eo2data[!duplicated(eo2data), ]
# only keep needed variables
eo2data <- eo2data[, c("org_id", "orgname", "fb_clean", "gwid")]



# Grouping for smdata for posts & global posts
smdata <- smdata %>%
  group_by(epr_groupid, year, country_gwid, accountname) %>%
  summarise(
    post_count = n(),
    post_count_global = sum(global ==1),
    .groups = "keep"
  ) %>%
  arrange(epr_groupid, year, country_gwid, accountname)

# Join smdata & eo2data
joined_data <- smdata %>%
  left_join(select(eo2data, org_id, orgname, fb_clean, gwid), by = c("accountname" = "fb_clean", "country_gwid" = "gwid"))


# Join smdata & eprdata
joined_data <- joined_data %>%
  left_join(select(eprdata, "Governmental power", "Regional autonomy", "Separatism/irredentism", "org_id", "year", "gwid", "groupid"), by = c("org_id","year", "country_gwid"="gwid", "epr_groupid"="groupid"))

 

# Proportion of global posts in total posts
joined_data$prop_global <- joined_data$post_count_global / joined_data$post_count

