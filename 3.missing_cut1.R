#------------------------------------------------------------------------------------------------#
#Build Dataset
#------------------------------------------------------------------------------------------------#  

rm(list = ls())

#2.a.domestic political
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.a.base_data.csv.gz"
base_data.2a <- fread(url)
rm(url)
#2.b.domestic economic
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.b.base_data.csv.gz"
base_data.2b <- fread(url)
rm(url)
#2.c.political instability
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.c.base_data.csv.gz"
base_data.2c <- fread(url)
rm(url)
#2.d.military variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.d.base_data.csv.gz"
base_data.2d <- fread(url)
rm(url)
#2.e.international variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.e.base_data.csv.gz"
base_data.2e <- fread(url)
rm(url)

base_data <- base_data.2a
rm(base_data.2a)
base_data.2b <- base_data.2b %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2b, by=c("ccode", "year", "month"))
rm(base_data.2b)
base_data.2c <- base_data.2c %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2c, by=c("ccode", "year", "month"))
rm(base_data.2c)    
base_data.2d <- base_data.2d %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2d, by=c("ccode", "year", "month"))
rm(base_data.2d)    
base_data.2e <- base_data.2e %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2e, by=c("ccode", "year", "month"))
rm(base_data.2e)

write.csv(base_data, "base_data.csv", row.names = FALSE)

#------------------------------------------------------------------------------------------------#
#Clean Base Data
#------------------------------------------------------------------------------------------------#  

base_data %>%
  count(country, ccode, year, month) %>%
  filter(n > 1)

# 12 instances of duplicates in base data. removed them below
base_data <- base_data %>%
  distinct(country, ccode, year, month, .keep_all = TRUE)

#------------------------------------------------------------------------------------------------#
#Begin filling N/As
#------------------------------------------------------------------------------------------------#  

# Source the interpolation functions script
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/3.interpolation.R")

# Check for missing values
values <- check_missing_values(base_data)

#------------------------------------------------------------------------------------------------#
#Filling N/As for Dummy Vars
#------------------------------------------------------------------------------------------------#
regime_vars <- c(
  "closed_autocracy",
  "electoral_autocracy",
  "electoral_democracy",
  "liberal_democracy"
)

# Backward fill missing values for dummy binary vars
for (v in regime_vars) {
  base_data <- backward_fill_before_first_obs(base_data, value_column = v)
}

# Forward fill missing values for dummy binary vars
for (v in regime_vars) {
  base_data <- forward_fill_after_last_obs(base_data, value_column = v)
}

#Interior gaps
for (v in regime_vars) {
  base_data <- fill_binary_interior_gaps(
    base_data,
    value_column = v
  )
}

base_data %>%
  filter(is.na(closed_autocracy)) %>%
  count(country, sort = TRUE)

#Remaining missing data is due to countries having no data at all

#------------------------------------------------------------------------------------------------#
#Filling N/As for Region
#------------------------------------------------------------------------------------------------#

base_data %>%
  filter(is.na(region)) %>%
  count(country, sort = TRUE)

# filling countries that have a couple N/As for region but is otherwise complete
base_data <- base_data %>%
  group_by(country) %>%
  mutate(region = ifelse(
    is.na(region),
    first(region[!is.na(region)]),
    region
  )) %>%
  ungroup()

#filling remaining region N/As based on v-dem classification where the original data was pulled
base_data <- base_data %>%
  mutate(region = case_when(
    
    country %in% c("Czechoslovakia",
                   "German Federal Republic") ~ 1,
    
    country %in% c("Bahamas", "Grenada", "Dominica",
                   "St. Lucia", "St. Vincent and the Grenadines",
                   "Antigua & Barbuda", "Belize",
                   "St. Kitts and Nevis") ~ 2,
    
    country == "Yemen Arab Republic" ~ 3,
    
    country %in% c("Liechtenstein", "San Marino",
                   "Andorra", "Monaco") ~ 5,
    
    country %in% c("Samoa", "Brunei",
                   "Federated States of Micronesia",
                   "Marshall Islands", "Palau",
                   "Kiribati", "Nauru",
                   "Tonga", "Tuvalu") ~ 6,
    
    TRUE ~ region   # keep existing values
  ))

#------------------------------------------------------------------------------------------------#
#Filling leader_duration
#------------------------------------------------------------------------------------------------#
#assuming that the current leader has stayed in power, filling n/as after last known observation
base_data <- base_data %>%
  arrange(country, year, month) %>%
  group_by(country) %>%
  mutate(
    last_obs = if (any(!is.na(Leader_duration))) {
      max(which(!is.na(Leader_duration)))
    } else {
      NA_integer_
    },
    
    Leader_duration = ifelse(
      !is.na(last_obs) &
        is.na(Leader_duration) &
        row_number() > last_obs,
      
      Leader_duration[last_obs] + (row_number() - last_obs),
      
      Leader_duration
    )
  ) %>%
  select(-last_obs) %>%
  ungroup()

#Back filling leader duration before first known value. It is restricted where if the leader duration hits 1 it will then go to N/A instead of 0 or a neg number
base_data <- base_data %>%
  arrange(country, year, month) %>%
  group_by(country) %>%
  mutate(
    first_obs = if (any(!is.na(Leader_duration))) {
      min(which(!is.na(Leader_duration)))
    } else {
      NA_integer_
    },
    
    Leader_duration = ifelse(
      !is.na(first_obs) &
        is.na(Leader_duration) &
        row_number() < first_obs,
      
      {
        proposed_value <- Leader_duration[first_obs] - 
          (first_obs - row_number())
        
        ifelse(proposed_value >= 0, proposed_value, NA)
      },
      
      Leader_duration
    )
  ) %>%
  select(-first_obs) %>%
  ungroup()

#Remaining N/As are due to either no data for country or no data before first known leader

#------------------------------------------------------------------------------------------------#
#Filling leader_age
#------------------------------------------------------------------------------------------------#
#Assuming current leader stays in power, continuing age after last known observation
base_data <- base_data %>%
  arrange(country, year, month) %>%
  group_by(country) %>%
  mutate(
    last_obs = if (any(!is.na(Leader_age))) {
      max(which(!is.na(Leader_age)))
    } else {
      NA_integer_
    },
    
    Leader_age = ifelse(
      !is.na(last_obs) &
        is.na(Leader_age) &
        row_number() > last_obs,
      
      Leader_age[last_obs] +
        floor((row_number() - last_obs) / 12),
      
      Leader_age
    )
  ) %>%
  select(-last_obs) %>%
  ungroup()

#Remaining N/As are due to either no data for country or no data before first known leader
