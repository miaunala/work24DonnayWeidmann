

```r
library(readr)
library(dplyr)
library(texreg)
library(fixest)
setwd("C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay")


# Read in data, create necessary variables, delete NAs and duplicates

# smdata
smdata <- read_csv("smdata.csv")
```

```
## Error: 'smdata.csv' does not exist in current working directory ('C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay').
```

```r
smdata$year <- as.numeric(substr(smdata$time, 7, 10))
```

```
## Error in substr(smdata$time, 7, 10): object 'smdata' not found
```

```r
smdata <- smdata %>% filter_at(vars(year, epr_groupid, country_gwid, accountname), all_vars(!is.na(.)))
```

```
## Error in tbl_vars_dispatch(x): object 'smdata' not found
```

```r
smdata <- smdata %>% filter(!duplicated(select(., index_n)))
```

```
## Error in filter(., !duplicated(select(., index_n))): object 'smdata' not found
```

```r
# eprdata
eprdata<- read_csv("eprdata.csv")
```

```
## Error: 'eprdata.csv' does not exist in current working directory ('C:/Users/whatt/Desktop/2024_ResAss_UZHIPZ_WeidmannDonnay').
```

```r
eprdata <- eprdata[!is.na(eprdata$orgname), ]
```

```
## Error in eval(expr, envir, enclos): object 'eprdata' not found
```

```r
eprdata <- eprdata[!duplicated(eprdata), ]
```

```
## Error in eval(expr, envir, enclos): object 'eprdata' not found
```

```r
# only keep needed variables
eprdata <- eprdata[, c("Governmental power", "Regional autonomy", "Separatism/irredentism", "org_id", "year", "gwid", "groupid")]
```

```
## Error in eval(expr, envir, enclos): object 'eprdata' not found
```

```r
# eo2data
eo2data <- read_csv("eo2_with_latest_publication_date.csv")
```

```
## Rows: 2122 Columns: 15
## -- Column specification ------------------------------------------------------------------
## Delimiter: ","
## chr  (11): org_id, orgname, URL, primary, channel_type, coding_date, comment, fb_clean...
## dbl   (2): gwid, confidence
## lgl   (1): month_recorded_latest_publication
## date  (1): latest_publication_date
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
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
    global_posts = sum(global ==1),
    .groups = "keep"
  ) %>%
  arrange(epr_groupid, year, country_gwid, accountname)
```

```
## Error in group_by(., epr_groupid, year, country_gwid, accountname): object 'smdata' not found
```

```r
# Join smdata & eo2data
joined_data <- smdata %>%
  left_join(select(eo2data, org_id, orgname, fb_clean, gwid), by = c("accountname" = "fb_clean", "country_gwid" = "gwid"))
```

```
## Error in left_join(., select(eo2data, org_id, orgname, fb_clean, gwid), : object 'smdata' not found
```

```r
# Join smdata & eprdata
joined_data <- joined_data %>%
  left_join(select(eprdata, "Governmental power", "Regional autonomy", "Separatism/irredentism", "org_id", "year", "gwid", "groupid"), by = c("org_id","year", "country_gwid"="gwid", "epr_groupid"="groupid"))
```

```
## Error in left_join(., select(eprdata, "Governmental power", "Regional autonomy", : object 'joined_data' not found
```

```r
# Proportion of global posts in total posts
joined_data$prop_global <- joined_data$global_posts / joined_data$post_count
```

```
## Error in eval(expr, envir, enclos): object 'joined_data' not found
```

