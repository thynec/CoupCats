rm(list = ls())

setwd("") # Set working file. 

source("packages.R") # Bring in packages. 
source("libraries.R") # 'Wake up' packages. 

base_data <- read_csv("base_data.csv") # Reading in base data. 

# 0. Country codes (Thyne 2022). 
url <- "https://www.uky.edu/~clthyn2/replace_ccode_country.xls" # Bringing in ccodes to merge. 
destfile <- "replace_ccode_country.xls"
curl::curl_download(url, destfile)
ccodes <- read_excel(destfile)
rm(url, destfile)

ccodes <- ccodes %>%
  subset(select = -c(ccode_polity, ccode_gw))
ccodes <- ccodes %>% # THESE NEED UPDATED! 
  filter(year == 2022) %>%
  mutate(year = 2023) %>%
  bind_rows(ccodes, .) 
ccodes <- ccodes %>%
  filter(year == 2022) %>%
  mutate(year = 2024) %>% 
  bind_rows(ccodes, .)

# 1. Coup data (Powell & Thyne 2011). 
# 1.1. Reading in data. 
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) 

# 1.2. Cleaning up data. 
coup_data <- coup_data %>% # I am getting rid of successes & fails--only if a coup was attempted! 
  select(-ccode_gw, -ccode_polity, -day, -version) %>% 
  mutate(coup_attempted = 1) %>% 
  select(-coup) %>% 
  distinct(ccode, year, month, .keep_all = TRUE) 

# 1.3. Merging into data set. 
base_data <- base_data %>% 
  left_join(coup_data, by = c("year", "country", "ccode", "month")) %>%
  mutate(coup_attempted = ifelse(is.na(coup_attempted), 0, as.numeric(coup_attempted))) # No NAs. 
label(base_data$coup_attempted) <- "2 = successful, 1 = failed"
rm(coup_data) # Keeping things clean! 
