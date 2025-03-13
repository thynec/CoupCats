#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building political instability variables

#Priority vars:
#protests using events data
#instability using Banks
#civil wars - COMPLETED
#Secondary vars:
#terrorist attacks

#1. clear all
rm(list = ls())
#2. set working directory
#setwd("~/R/coupcats") # Set working file. 
#setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/current_research/coupcats") #clay at work
#3. install packages
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#------------------------------------------------------------------------------------------------#
#civil wars from UCDP/PRIO ACD; https://ucdp.uu.se/downloads/index.html#armedconflict
#------------------------------------------------------------------------------------------------#  

#bring in data
url <- "https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-241-xlsx.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
cw <- read_excel("data/UcdpPrioConflict_v24_1.xlsx")
unlink("data", recursive=TRUE)
rm(url)  

#clean data; only type 3 and 4 CWs
cw <- cw %>%
  filter(type_of_conflict=="3" | type_of_conflict=="4") %>%
  mutate(loc = as.numeric(gwno_loc)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(loc!=2) %>% #US in civil war is miscoded
  select(location, year, loc) %>%
  rename(country=location) 
cw <- cw %>%
  left_join(ccodes, by=c("country", "year"))
check <- cw %>%
  filter(loc!=ccode | is.na(ccode)) #need to fix Madagascar
table(check$country)
rm(check)
cw <- cw %>%
  mutate(ccode=ifelse(country=="Madagascar (Malagasy)", 580, ccode))
check2 <- cw %>%
  filter(loc!=ccode | is.na(ccode))
table(check2$country) #we're good
rm(check2)
cw <- cw %>%
  mutate(year=year+1) %>% #just lagged
  rename(mcountry=country) %>%
  select(-loc) %>%
  mutate(cw=1) %>%
  distinct() 
summary(cw$year) #fill missing years from 1947-2024
#merge into base
base_data <- base_data %>%
  left_join(cw, by=c("ccode", "year"))
check <- base_data %>%
  filter(mcountry!=country) %>%
  select(country, mcountry) %>%
  distinct() 
View(check) #looks good
rm(check, cw)
base_data <- base_data %>%
  mutate(cw=ifelse(is.na(cw) & year>=1947 & year<=2024, 0, cw)) %>%
  set_variable_labels(cw="3-4 types from UCDP, t-1") %>%
  select(-mcountry)

# -------------------------- Political Stability (From World Bank) ----------------------------- #
#1.1 Getting the data
url <- "https://api.worldbank.org/v2/en/indicator/PV.EST?downloadformat=excel"
destfile <- "PV.xls"
curl::curl_download(url, destfile)
stability <- read_excel(destfile, skip = 3)
rm(destfile, url)

#1.2 Reshaping data
stability <- stability %>%
  rename( "country" = `Country Name`) %>% 
  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`))  #removing things we don't want
stability <- stability %>%
  pivot_longer(
    cols = -c(country),  # Keep country-related columns fixed
    names_to = "year",  
    values_to = "stability"
  ) %>%
  mutate(year = as.integer(year))  # Convert Year to integer
stability <- stability %>%
  mutate(year=year+1) #just lagged

#checking values
check <- stability %>%
  filter(!is.na(stability))
summary(check) #goes from 1997 to 2024
rm(check)
check2 <- stability %>%
  filter(year>=1997 & year<=2024) %>%
  filter(is.na(stability))
table(stability$country) #missing a lot of country/years even when pared down to 1997-2024
hist(stability$stability) #distribution looks good
rm(check2)

#1.3 Merging to base_data
stability <- stability %>%
  left_join(ccodes, by=c("country", "year")) %>%
  dplyr::select(-country) %>%
  distinct()
base_data <- base_data %>%
  left_join(stability, by=c("ccode", "year"))
rm(stability)


###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.c.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  




