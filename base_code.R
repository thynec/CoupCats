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
