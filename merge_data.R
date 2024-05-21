library(readr)
library(dplyr)
setwd("C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay")


# this is the original dataset where topic models and translations were run (without all additional columns)
#org_eo2_sample <- read_csv("data/data_sample_eo2_tw_fb_all.csv.gz")

# this is the original dataset with all additional columns
#org_eo2_original <- read_csv("data/data_sample_eo2_tw_fb_all_original_data.csv.gz")

example_data <- read_csv("example_data.csv")
epro_t_organisv<- read_csv("epro_t.csv")
gesis_eo2_latest_pub <- read_csv("eo2_with_latest_publication_date.csv")



# for example_data
results_example_data <- example_data %>%
  group_by(epr_groupid, year = substr(time, 7, 10), country_gwid, accountname) %>%
  summarise(
    post_count = n(),
    post_count_global = sum(global ==1),
    .groups = "keep"
  ) %>%
  arrange(epr_groupid, year, country_gwid, accountname)

# needed to filter out where epr_groupid is not there as NAs would have been 
# matched with the post count otherwise during the left join.
results_example_data <- results_example_data %>%
  filter(!is.na(epr_groupid))

duplicated_rows1 <- results_example_data[duplicated(results_example_data), ]
print(duplicated_rows1)


res_ex_JOIN_eo2_latPub <- results_example_data %>%
  left_join(select(gesis_eo2_latest_pub, org_id, orgname, fb_clean), by = c("accountname" = "fb_clean"))


res_ex_JOIN_eo2_latPub$year <- as.numeric(res_ex_JOIN_eo2_latPub$year)

duplicated_rows2 <- res_ex_JOIN_eo2_latPub[duplicated(res_ex_JOIN_eo2_latPub), ]
print(duplicated_rows2)


final_ex_eo2_epro <- res_ex_JOIN_eo2_latPub %>%
  left_join(select(epro_t_organisv, "Governmental power", "Regional autonomy", "Separatism/irredentism", "org_id", "year"), by = c("org_id","year"))

duplicated_rows3 <- final_ex_eo2_epro[duplicated(final_ex_eo2_epro), ]
print(duplicated_rows3)

final_ex_eo2_epro_distinct <- final_ex_eo2_epro %>%
  distinct()

