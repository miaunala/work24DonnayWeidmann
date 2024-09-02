library(readr)
library(dplyr)
library(texreg)
library(fixest)

# for VM
#setwd("/data/nguibe")
#setwd("C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay")


# Read in data, create necessary variables, delete NAs and duplicates

# smdata
smdata <- read_csv("data/example_data.csv")
smdata$year <- as.numeric(substr(smdata$time, 7, 10))
smdata <- smdata %>% filter_at(vars(year, epr_groupid, country_gwid, accountname), all_vars(!is.na(.)))
smdata <- smdata %>% filter(!duplicated(select(., index_n)))


# eprdata
eprdata<- read_csv("data/epro_t.csv")
# delete missings
eprdata <- eprdata[!is.na(eprdata$orgname), ]
# delete duplicates
eprdata <- eprdata[!duplicated(eprdata), ]
# rename variables
eprdata <- eprdata %>%
  rename(gov_pow = `Governmental power`) %>%
  rename(reg_autonom = `Regional autonomy`) %>%
  rename(sep_irred = `Separatism/irredentism`) %>%
  rename(eth_claim = `Ethnic claim`)
# only keep needed variables
eprdata <- eprdata[, c("gov_pow", "reg_autonom", "sep_irred", "org_id", "year", "gwid", "groupid", "eth_claim")]

# eo2data
eo2data <- read_csv("data/data_sample_eo2_tw_fb_all_original_data.csv.gz")
# delete missings
eo2data <- eo2data[!is.na(eo2data$orgname),]
# only keep needed variables
eo2data <- eo2data[, c("org_id_from_coll", "orgname", "accountname", "gwid.x", "groupid", "year")]

# power access & conflict data
powacc_confdata <- read_csv("data/powacc_conflict.csv")
# removing missings
powacc_confdata <- powacc_confdata[!is.na(powacc_confdata$groupname),]
# only keep needed variables
powacc_confdata <- powacc_confdata[, c("warhist", "peaceyears", "status_excl", "onset_ko_flag", "onset_do_flag", "incidence_flag", "year", "gwgroupid", "groupname")]

# Grouping for smdata for posts & global posts
smdata <- smdata %>%
  group_by(epr_groupid, year, country_gwid, accountname) %>%
  summarise(
    post_count = n(),
    global_posts = sum(global ==1),
    .groups = "keep"
  ) %>%
  arrange(epr_groupid, year, country_gwid, accountname)


# Join Social Media data with EO2 data & remove duplicates
joined_data <- smdata %>%
  left_join(select(eo2data, org_id_from_coll, orgname, accountname, gwid.x, year, groupid), by = c("accountname" = "accountname", "country_gwid" = "gwid.x", "year", "epr_groupid"="groupid"))
joined_data <- joined_data[!duplicated(joined_data),]


# Join previously joined_data with eprdata
joined_data <- joined_data %>%
  left_join(select(eprdata, "gov_pow", "reg_autonom", "sep_irred", "org_id", "year", "gwid", "groupid", "eth_claim"), by = c("org_id_from_coll"="org_id","year", "country_gwid"="gwid", "epr_groupid"="groupid"))
 

# Join previously joined_data with Power Access and Conflict Data
joined_data <- joined_data %>% left_join(select(powacc_confdata, "warhist", "peaceyears", "status_excl", "onset_ko_flag", "onset_do_flag", "incidence_flag", "year", "gwgroupid", "groupname"), by = c("epr_groupid"="gwgroupid", "year"))


# Proportion of global posts in total posts
joined_data$prop_global <- joined_data$global_posts / joined_data$post_count

# Save joined_data as CSV
write_csv(x = joined_data, "data/joined_data.csv")




