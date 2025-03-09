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
  #setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/current_research/coupcats") #clay at work
#3. install packages
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
  source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#------------------------------------------------------------------------------------------------#
#add regions
#------------------------------------------------------------------------------------------------#  

  
#get regions from WDI
  df <- WDI(indicator = c("CC.EST", "CC.PER.RNK"), country = "all", start = 1996, end = 2023, extra = TRUE)
  df <- df %>%
    select(country, region) %>%
    distinct() %>%
    left_join(ccodes, by=c("country"))
  check <- df %>%
    filter(is.na(ccode))
  table(check$country) #add Turkey=ccode=640
  df <- df %>%
    mutate(ccode=ifelse(country=="Turkiye", 640, ccode))
  check <- df %>%
    filter(is.na(ccode))
  table(check$country) #all good
  rm(check)
  df <- df %>%
    select(-year) %>%
    distinct()
  df <- df %>%
    rename(wdi_region=region) %>%
    mutate(e_asia_pacific=ifelse(wdi_region=="East Asia & Pacific", 1, 0)) %>%
    mutate(euro_cent_asia=ifelse(wdi_region=="Europe & Central Asia", 1, 0)) %>%
    mutate(LA_carrib=ifelse(wdi_region=="Latin America & Caribbean", 1, 0)) %>%
    mutate(MENA=ifelse(wdi_region=="Middle East & North Africa", 1, 0)) %>%
    mutate(N_america=ifelse(wdi_region=="North America", 1, 0)) %>%
    mutate(S_asia=ifelse(wdi_region=="South Asia", 1, 0)) %>%
    mutate(Sub_africa=ifelse(wdi_region=="Sub-Saharan Africa", 1, 0)) %>%
    set_variable_labels(
      e_asia_pacific="East Asia & Pacific",
      euro_cent_asia="Europe & Central Asia",
      LA_carrib="Latin America & Caribbean",
      MENA="Middle East & North Africa",
      N_america="North America",
      S_asia="South Asia",
      Sub_africa="Sub-Saharan Africa") %>%
    select(-country, -wdi_region)
  base <- base_data %>%
    left_join(df, by=c("ccode"))
  check <- base %>%
    filter(is.na(e_asia_pacific)) %>%
    select(country, year) %>%
    distinct()
  
  
  
  %>%
    select(-wdi_region)
  
  
  
  
  
  
  
  # 1.2. Cleaning up data. 
  wdi_data <- wdi_data %>%
    mutate(year=year+1) %>% #just lagged by adding a year
    subset(select = c(country, year, CC.EST, CC.PER.RNK, region)) %>% 
    rename(corruption_est = CC.EST, 
           corruption_rank = CC.PER.RNK,
           wdi_region = region) %>%
    set_variable_labels(
      corruption_est="Control of Corruption: Estimate; from WDI; t-1",
      corruption_rank="Control of Corruption: Percentile Rank; from WDI; t-1",
      wdi_region="regions from WDI"
    )
  wdi_data <- wdi_data %>%
    left_join(ccodes, by = c("year", "country")) 
  check <- wdi_data %>%
    filter(if_any(everything(), is.na)) 
  table(check$country)
  table(check$year)
  #check looks fine; missing 3 in normal places due to temporal coverage; other missings are due to small countries or regions
  rm(check) 
  
  # 1.3. Merging into data set. Do this twice. First for corruption by ccode/year; then for region by ccode (region doesn't vary by time)
  wdi1 <- wdi_data %>%
    select(-country, -wdi_region)
  base_data <- base_data %>% 
    left_join(wdi1, by = c("year", "ccode")) # NAs are unreported years. 
  wdi2 <- wdi_data %>%
    select(ccode, wdi_region) %>%
    distinct()
  base_data <- base_data %>%
    left_join(wdi2, by=c("ccode"))
  rm(wdi_data, wdi1, wdi2)
  
  #Add regional dummies

  


















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





###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.e.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  




