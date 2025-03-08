#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building international-related variables
#Priority vars:
  #trade
  #cold war dummy
  #ongoing interstate war
  #regional contagion
  #regional dummies
  #FDI
#Secondary variables:
  #foreign military training (McLauchlin/Seymour/Martel J. Peace Research 2022)
  #international signals
  #rivalries (Bak/Chavez/Rider J. Conflict Resolution 2019)
  #defensive alliances (Boutton Intl Studies Quarterly 2019)
  #peacekeeping (Banini/Powell/Yekple African Security 2020)

#1. clear all
  rm(list = ls())
#2. set working directory
  #setwd("~/R/coupcats") # Set working file. 
  #setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
#3. install packages
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
  source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#------------------------------------------------------------------------------------------------#
#put all new/revised coding below
#------------------------------------------------------------------------------------------------#  

# -------------------------- Int Signals ----------------------------- #
  
# 1. WEIS DATA
# 1.1. Getting the data
  url <- "https://github.com/thynec/CoupCats/raw/main/weis.fromlai.1966-93.dta" #bringing in the data
  weis <- read_dta(url)
  rm(url)
  
# 1.2. Cleaning
  weis <- weis %>% 
    dplyr::select(year,targcc, mo, gscale) %>% #selecting important variables
    rename(
      target = targcc,  
      month = mo   
    ) %>% 
    mutate("z_variable" = scale(weis$gscale)) #getting the z score
  
# 1.3. Aggregating by month    # We figured that it is possibly better to do this after we merge both datasets and have them in the same scale
  # weis <- weis %>%
  #group_by(year, month, target) %>%
  #summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") #aggregating by month, year, and target
  
# 2. COPDAB DATA
# 2.1. Getting the data
  library(haven) # we needed this for copdab
  url <- "https://www.uky.edu/~clthyn2/copdab.dta"
  copdab <- read_dta(url(url, "rb"))
  rm(url)
  
# 2.2. Cleaning and Fixing year
  copdab <- copdab %>% 
    dplyr::select(year,target, month, value) %>% #selecting important variables
    filter(target != 4)
  
  copdab <- copdab %>%
    mutate(year = 1900 + year)
  
# 2.3. Turning values into something meaningful 
  copdab <- copdab %>%
    mutate(weight = case_when(
      value == 15 ~ -102,
      value == 14 ~ -65,
      value == 13 ~ -50,
      value == 12 ~ -44,
      value == 11 ~ -29,
      value == 10 ~ -16,
      value == 9 ~ -6,    # Negative for values from 15 to 9
      value == 8 ~ 1,
      value == 7 ~ 6,
      value == 6 ~ 10,
      value == 5 ~ 14,
      value == 4 ~ 27,
      value == 3 ~ 31,
      value == 2 ~ 47,
      value == 1 ~ 92,
      TRUE ~ NA_real_ # Ensures NA for any value outside the expected range
    )) %>% 
    select(-value)
  
  copdab <-  copdab %>% 
    mutate("z_variable" = scale(copdab$weight))  # getting the z score


