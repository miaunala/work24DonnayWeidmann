library(readr)
library(dplyr)
library(texreg)
library(fixest)

# for VM
setwd("/data/nguibe")
#setwd("C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay")


# Read in data, create necessary variables, delete NAs and duplicates

# smdata
smdata <- read_csv("data/example_data.csv")
smdata$year <- as.numeric(substr(smdata$time, 7, 10))
smdata <- smdata %>% filter_at(vars(year, epr_groupid, country_gwid, accountname), all_vars(!is.na(.)))
smdata <- smdata %>% filter(!duplicated(select(., index_n)))


# eprdata
eprdata<- read_csv("data/epro_t.csv")
eprdata <- eprdata[!is.na(eprdata$orgname), ]
eprdata <- eprdata[!duplicated(eprdata), ]
# only keep needed variables
eprdata <- eprdata[, c("Governmental power", "Regional autonomy", "Separatism/irredentism", "org_id", "year", "gwid", "groupid")]
# rename complicated variables
eprdata <- eprdata %>%
  rename(gov_pow = `Governmental power`) %>%
  rename(reg_autonom = `Regional autonomy`) %>%
  rename(sep_irred = `Separatism/irredentism`)

# eo2data old
eo2data_old <- read_csv("data/eo2_with_latest_publication_date.csv")
eo2data_old <- eo2data_old[!is.na(eo2data_old$orgname), ]
eo2data_old <- eo2data_old %>% filter(!duplicated(select(., org_id, clean_username, channel_type)))
#eo2data <- eo2data[!duplicated(eo2data), ]
# only keep needed variables
eo2data_old <- eo2data_old[, c("org_id", "orgname", "fb_clean", "gwid")]


# eo2data new
eo2data <- read_csv("data/data_sample_eo2_tw_fb_all_original_data.csv.gz")
eo2data <- eo2data[!is.na(eo2data$orgname),]
#org_id [org_id_from_coll], clean_username [accountname], channel type exists muss noch, 
# keep only needed variables
# org_id_from_coll, orgname, accountname, gwid.x
eo2data <- eo2data[, c("org_id_from_coll", "orgname", "accountname", "gwid.x")]


# Grouping for smdata for posts & global posts
smdata <- smdata %>%
  group_by(epr_groupid, year, country_gwid, accountname) %>%
  summarise(
    post_count = n(),
    global_posts = sum(global ==1),
    .groups = "keep"
  ) %>%
  arrange(epr_groupid, year, country_gwid, accountname)

# Join smdata & eo2data
#old data
joined_data_old <- smdata %>%
  left_join(select(eo2data_old, org_id, orgname, fb_clean, gwid), by = c("accountname" = "fb_clean", "country_gwid" = "gwid"))
#new data
joined_data <- smdata %>%
  left_join(select(eo2data, org_id_from_coll, orgname, accountname, gwid.x), by = c("accountname" = "accountname", "country_gwid" = "gwid.x"), relationship = "many-to-many")


# Join smdata & eprdata
joined_data_old <- joined_data_old %>%
  left_join(select(eprdata, "Governmental power", "Regional autonomy", "Separatism/irredentism", "org_id", "year", "gwid", "groupid"), by = c("org_id","year", "country_gwid"="gwid", "epr_groupid"="groupid"))
joined_data <- joined_data %>%
  left_join(select(eprdata, "gov_pow", "reg_autonom", "sep_irred", "org_id", "year", "gwid", "groupid"), by = c("org_id","year", "country_gwid"="gwid", "epr_groupid"="groupid"))
 

# Proportion of global posts in total posts
joined_data$prop_global <- joined_data$global_posts / joined_data$post_count



