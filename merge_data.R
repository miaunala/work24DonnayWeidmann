library(readr)
library(dplyr)
library(texreg)
library(fixest)
setwd("C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay")


# This is the original dataset where topic models and translations were run (without all additional columns)
#org_eo2_sample <- read_csv("data/data_sample_eo2_tw_fb_all.csv.gz")

# this is the original dataset with all additional columns
#org_eo2_original <- read_csv("data/data_sample_eo2_tw_fb_all_original_data.csv.gz")

# Read in data, create necessary variables, delete NAs and duplicates

# example_data
example_data <- read_csv("example_data.csv")
example_data$year <- as.numeric(substr(example_data$time, 7, 10))
example_data <- example_data %>% filter_at(vars(year, epr_groupid, country_gwid, accountname), all_vars(!is.na(.)))
example_data <- example_data %>% filter(!duplicated(select(., index_n)))
#Alternative version: example_data <- example_data[!duplicated(example_data), ]

# epro_t
epro_t<- read_csv("epro_t.csv")
epro_t <- epro_t[!is.na(epro_t$orgname), ]
epro_t <- epro_t[!duplicated(epro_t), ]

# gesis_eo
gesis_eo <- read_csv("eo2_with_latest_publication_date.csv")
gesis_eo <- gesis_eo[!is.na(gesis_eo$orgname), ]
gesis_eo <- gesis_eo %>% filter(!duplicated(select(., org_id, clean_username, channel_type)))
#gesis_eo <- gesis_eo[!duplicated(gesis_eo), ]



# Grouping for example_data for posts & global posts
example_data <- example_data %>%
  group_by(epr_groupid, year, country_gwid, accountname) %>%
  summarise(
    post_count = n(),
    post_count_global = sum(global ==1),
    .groups = "keep"
  ) %>%
  arrange(epr_groupid, year, country_gwid, accountname)
example_data <- example_data[!duplicated(example_data), ]

# Join example_data & gesis_eo
joined_data <- example_data %>%
  left_join(select(gesis_eo, org_id, orgname, fb_clean, gwid), by = c("accountname" = "fb_clean", "country_gwid" = "gwid"))
joined_data <- joined_data[!duplicated(joined_data), ]

# Join example_data & epro_t
joined_data <- joined_data %>%
  left_join(select(epro_t, "Governmental power", "Regional autonomy", "Separatism/irredentism", "org_id", "year", "gwid"), by = c("org_id","year", "country_gwid"="gwid"))
joined_data <- joined_data[!duplicated(joined_data), ]
 

# Proportion of global posts in total posts
joined_data$prop_global <- joined_data$post_count_global / joined_data$post_count

# Linear regression w/ fixed effects for country and year
model1 <- feols(prop_global ~ `Regional autonomy` + `Separatism/irredentism` | country_gwid + year, data = joined_data)
model1
