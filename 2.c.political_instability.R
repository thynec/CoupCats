#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building political instability variables

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

#1.3 Merging to base_data
stability <- stability %>%
  left_join(ccodes, by=c("country", "year")) %>%
  dplyr::select(-country) %>%
  distinct()
base_data <- base_data %>%
  left_join(stability, by=c("ccode", "year"))
rm(stability)

# -------------------------- Mass Mobilization (V-Dem) ----------------------------- #

# Bringing in data. 
vdem_data <- vdem

# Cleaning up data. 
vdem_data <- vdem %>%
  subset(select = c(country_name, # Country. 
                    year, # Year. 
                    v2cagenmob, # Mass mobilization (ordinal, converted to interval; Z-score). 
                    v2caconmob)) %>% # Mass mobilization concentration (ordinal, converted to interval; Z-score). 
  rename(country = country_name,
         year = year, 
         mobilization = v2cagenmob,
         mobil_conc = v2caconmob) %>% 
  mutate(year=year+1) %>% # Just lagged. 
  filter(year >= 1950) 
vdem_data <- vdem_data %>% # Merging in ccodes. 
  left_join(ccodes, by = c("year", "country")) 
vdem_data <- vdem_data %>%
  filter(!is.na(ccode)) # No Republic of Vietnam (1950-76), Afghanistan (1950-2000), Guinea-Bissau (1950-2005), Laos (2024), Iceland (1950-2019)

# Merging into data set. 
check <- vdem_data %>%
  arrange(ccode, year) %>%
  filter(ccode==lag(ccode) & year==lag(year)) #0 so we're good
rm(check)

vdem_data <- vdem_data %>%
  rename(mcountry = country)
base_data <- base_data %>%
  left_join(vdem_data, by=c("ccode", "year"))
rm(vdem_data)
base_data <- base_data %>%
  select(-mcountry)

#------------------------------------------------------------------------------------------------#  
#add in protest data from acled
#------------------------------------------------------------------------------------------------#  

#download 'Number of political violence events by country-month-year' from https://acleddata.com/aggregated/number-political-violence-events-country-month-year
#note that need to log in to download these, so downloaded on 03/10/26 and put on github; will need to grab data every time we need to update
url <- "https://github.com/thynec/CoupCats/raw/refs/heads/data/number_of_political_violence_events_by_country-month-year_as-of-27Feb2026.xlsx"
destfile <- "number_of_political_violence_events_by_country_month_year_as_of_27Feb2026.xlsx"
curl::curl_download(url, destfile)
acled <- read_excel(destfile)
rm(destfile, url)
acled <- acled %>%
  rename(country=COUNTRY,
         month=MONTH,
         year=YEAR,
         acled=EVENTS) %>%
  set_variable_labels(acled="pol violence events by month") %>% 
  mutate(month=match(month, month.name)) %>%
  mutate(date = ymd(paste(year, month, "01"))) %>%
  mutate(date=date %m+% months(1)) %>%
  select(-month, -year) %>%
  mutate(year=year(date)) %>%
  mutate(month=month(date)) %>%
  select(-date)
acled <- acled %>%
  left_join(ccodes, by=c("country", "year"))
check <- acled %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct()
#all missing ccodes are weird ones; fine to ignore
rm(check)
acled <- acled %>%
  select(-country)
base_data <- base_data %>%
  left_join(acled, by=c("ccode", "year", "month"))
rm(acled)

#compare acled to vdem
check <- base_data %>%
  mutate(acled=log(acled+100))
cor(check %>% 
      select(mobilization, mobil_conc, acled),
    use = "complete.obs")













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
cw <- cw %>%
  mutate(ccode=ifelse(country=="Madagascar (Malagasy)", 580, ccode))
cw <- cw %>%
  mutate(year=year+1) %>% #just lagged
  rename(mcountry=country) %>%
  select(-loc) %>%
  mutate(cw=1) %>%
  distinct() 
summary(cw$year) #fill missing years from 1947-2024
#expand for 2025; assuming ongoing CWs in 2024 continue to 2025
cw <- cw %>%
  mutate(expand=ifelse(year==2024, 2, 1)) %>%
  uncount(expand) %>%
  arrange(ccode, year) %>%
  mutate(year=ifelse(year==2024 & lag(year)==2024 & ccode==lag(ccode), 2025, year))
#merge into base
base_data <- base_data %>%
  left_join(cw, by=c("ccode", "year"))
rm(cw)
base_data <- base_data %>%
  mutate(cw=ifelse(is.na(cw) & year>=1947 & year<=2025, 0, cw)) %>%
  set_variable_labels(cw="3-4 types from UCDP, t-1") %>%
  select(-mcountry)

#------------------------------------------------------------------------------------------------#
#civil conflict severity (battle-related deaths) from UCDP; https://ucdp.uu.se/downloads/
#------------------------------------------------------------------------------------------------#  

#bring in data
url <- "https://ucdp.uu.se/downloads/brd/ucdp-brd-conf-251-xlsx.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
brd <- read_excel(list.files("data", pattern="\\.xlsx$", full.names=TRUE)[1])
unlink("data", recursive=TRUE)
rm(url)

#clean data; only type 3 and 4 conflicts; aggregate deaths to country-year
brd <- brd %>%
  filter(type_of_conflict=="3" | type_of_conflict=="4") %>%
  mutate(year=as.numeric(year)) %>%
  rename(country=location_inc) %>%
  group_by(country, year) %>%
  summarise(brd=sum(bd_best, na.rm=TRUE), .groups="drop")
brd <- brd %>%
  left_join(ccodes, by=c("country", "year"))
brd <- brd %>%
  filter(!is.na(ccode)) %>%
  mutate(year=year+1) %>% #just lagged
  distinct()
#expand for 2025; assume that conflicts in 2024 continue to 2025
brd <- brd %>%
  mutate(expand=ifelse(year==2024, 2, 1)) %>%
  uncount(expand) %>%
  arrange(ccode, year) %>%
  mutate(year=ifelse(year==2024 & lag(year)==2024 & ccode==lag(ccode), 2025, year)) %>%
  group_by(ccode, year) %>%
  summarise(brd=sum(brd, na.rm=TRUE), .groups="drop") %>%
  select(ccode, year, brd)
#merge
base_data <- base_data %>%
  left_join(brd, by=c("ccode", "year"))
rm(brd)
base_data <- base_data %>%
  mutate(brd=ifelse(is.na(brd) & year>=1989 & year<=2025, 0, brd)) %>%
  set_variable_labels(brd="battle-related deaths, types 3-4, UCDP, t-1")










###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.c.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  





















