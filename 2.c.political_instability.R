#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building political instability variables

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

#1.25 Recode World Bank names to ccodes
stability <- stability %>%
  mutate(country = case_when(
    country == "Germany"                          ~ "German Federal Republic",  # WB uses unified Germany throughout; split needed pre-1990
    country == "Korea, Rep."                      ~ "South Korea",
    country == "Korea, Dem. People's Rep."        ~ "North Korea",
    country == "Yemen, Rep."                      ~ "Yemen Arab Republic",
    country == "Yemen, PDR"                       ~ "Yemen People's Republic",
    country == "Viet Nam"                         ~ "Vietnam",
    country == "Turkiye"                          ~ "Turkey",
    country == "Eswatini"                         ~ "Swaziland",
    country == "Congo, Dem. Rep."                 ~ "Democratic Republic of the Congo",
    country == "Congo, Rep."                      ~ "Congo",
    country == "Micronesia, Fed. Sts."            ~ "Federated States of Micronesia",
    country == "St. Kitts and Nevis"              ~ "St. Kitts and Nevis",
    country == "São Tomé and Príncipe"            ~ "Sao Tome and Principe",
    TRUE                                          ~ country
  )) %>%
    # West Germany only exists pre-1991 — post-1990 "Germany" rows stay as unified Germany
    mutate(country = case_when(
      country == "German Federal Republic" & year > 1990 ~ "Germany",
      TRUE ~ country
    ))

#1.3 Merging to base_data
stability <- stability %>%
  left_join(ccodes, by=c("country", "year")) 
check <- stability %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct() #need to add somalia=520; turkiye=640
rm(check)
stability <- stability %>%
  mutate(ccode=ifelse(country=="Somalia, Fed. Rep.", 520, ccode)) %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  filter(!is.na(ccode)) %>%
  mutate(month=12) %>%
  set_variable_labels(stability="from WB, t-1") %>%
  select(-country)
base_data <- base_data %>%
  left_join(stability, by=c("ccode", "year", "month"))
rm(stability)

#1.4 Interpolation
summary(base_data$stability) #range: -3.313, 1.759
stab <- base_data %>%
  filter(!is.na(stability))
summary(stab$year) #1997 - 2024
rm(stab)
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(stability_OG = stability) %>%
  select(-stability) %>%
  set_variable_labels(stability_OG = "from WB, t-1, original") %>%
  mutate(stability_WB = na.approx(stability_OG, x=row_number(), na.rm=FALSE, rule=2)) %>%
  mutate(stability_WB=ifelse(stability_WB < -3.313, -3.313, stability_WB)) %>%
  mutate(stability_WB=ifelse(stability_WB > 1.759, 1.759, stability_WB))

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
  filter(year >= 1950) %>%
  mutate(country = case_when(
    country == "Republic of Vietnam"              ~ "Republic of Vietnam",   
    country == "German Federal Republic"          ~ "German Federal Republic",
    country == "German Democratic Republic"       ~ "German Democratic Republic",
    country == "Yemen Arab Republic"              ~ "Yemen Arab Republic",
    country == "Yemen People's Republic"          ~ "Yemen People's Republic",
    country == "Czechoslovakia"                   ~ "Czechoslovakia",
    TRUE                                          ~ country
  )) 
vdem_data <- vdem_data %>% # Merging in ccodes. 
  left_join(ccodes, by = c("year", "country")) 
check <- vdem_data %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct() #all good
rm(check)
vdem_data <- vdem_data %>%
  filter(!is.na(ccode)) %>%
  mutate(month=12) %>%
  select(-country)
base_data <- base_data %>%
  left_join(vdem_data, by=c("ccode", "year", "month"))
rm(vdem_data)

#interpolate
mobil <- base_data %>%
  filter(!is.na(mobilization))
summary(mobil) #1950-2025; #mobilization range: -3.5680, 4.0130; mobil_conc range: -3.0710, 3.7370
rm(mobil)
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(mobilization_OG=mobilization) %>%
  mutate(mobil_conc_OG=mobil_conc) %>%
  set_variable_labels(mobilization_OG="mobilization, vdem, t-1, OG") %>%
  set_variable_labels(mobil_conc_OG="mobil_conc, vdem, t-1, OG") %>%
  mutate(mobilization = na.approx(mobilization, x=row_number(), na.rm=FALSE, rule=2)) %>%
  mutate(mobilization=ifelse(mobilization < -3.5680, -3.5680, mobilization)) %>%
  mutate(mobilization=ifelse(mobilization > 4.0130, 4.0130, mobilization)) %>%
  mutate(mobil_conc = na.approx(mobil_conc, x=row_number(), na.rm=FALSE, rule=2)) %>%
  mutate(mobil_conc=ifelse(mobil_conc < -3.0710, -3.0710, mobil_conc)) %>%
  mutate(mobil_conc=ifelse(mobil_conc > 3.737, 3.737, mobil_conc))

#------------------------------------------------------------------------------------------------#  
#add in protest data from acled
#------------------------------------------------------------------------------------------------#  

#download 'Number of political violence events by country-month-year' from https://acleddata.com/aggregated/number-political-violence-events-country-month-year
#note that need to log in to download these, so downloaded on 03/10/26 and put on github; will need to grab data every time we need to update
url <- "https://github.com/thynec/CoupCats/raw/refs/heads/data/acled.xlsx"
destfile <- "acled.xlsx"
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
  mutate(date=date %m+% months(1)) %>% #just lagged by 1 month
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

#try to splice these
check <- base_data %>%
  filter(!is.na(acled))
summary(check$year) #1997 - 2026
rm(check)
base_data <- base_data %>%
  mutate(acledl=log10(acled+1)) %>%
  ungroup() %>%
  mutate(acledlz=as.numeric(scale(acledl))) %>%
  set_variable_labels(acledlz = "acled, log10, t-1, Z") %>%
  mutate(stability_WB=stability_WB*-1) %>%
  mutate(stability_OG=stability_OG*-1)
cor(base_data %>% 
      select(mobilization_OG, mobil_conc_OG, acledlz, stability_OG, stability_WB),
    use = "complete.obs")
#WB and ACLED seem pretty reasonable; look at some examples...
countries_to_plot <- c("United States", "Iran", "Ivory Coast", "South Africa")
test <- base_data %>% filter(year>1995)
plot_df <- test %>%
  filter(country %in% countries_to_plot) %>%
  mutate(date = lubridate::ymd(paste(year, month, "01"))) %>%
  select(country, date, stability_WB, acledlz) %>%
  pivot_longer(
    cols = c(stability_WB, acledlz),
    names_to = "measure",
    values_to = "value"
  )
ggplot(plot_df, aes(x = date, y = value, color = measure)) +
  geom_line(size = 1) +
  facet_wrap(~country, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Stability vs ACLED (Z-scores)",
    x = "Year",
    y = "Z-score",
    color = "Measure"
  )
rm(plot_df)
rm(countries_to_plot)
rm(test)
#go with acledlz if it exists; WB if not
base_data <- base_data %>%
  mutate(protests=stability_WB) %>%
  mutate(protests=ifelse(!is.na(acledlz), acledlz, protests)) %>%
  rename(acled_OG=acled) %>%
  set_variable_labels(acledl = "acled, t-1, log10") %>%
  set_variable_labels(acled_OG = "acled, t-1, original") %>%
  set_variable_labels(protests="acled+WB splice, t-1, Z")

#------------------------------------------------------------------------------------------------#
#civil wars from UCDP/PRIO ACD; https://ucdp.uu.se/downloads/index.html#armedconflict
#------------------------------------------------------------------------------------------------#  

#bring in data
url <- "https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-251-xlsx.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
cw <- read_excel("data/UcdpPrioConflict_v25_1.xlsx")
unlink("data", recursive=TRUE)
rm(url)  

#clean data; only type 3 and 4 CWs
cw <- cw %>%
  filter(type_of_conflict=="3" | type_of_conflict=="4") %>%
  mutate(loc = as.numeric(gwno_loc)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(loc!=2) %>% #US in civil war is miscoded
  select(location, year, loc, start_date2, ep_end_date) %>%
  mutate(ep_end_date = coalesce(ep_end_date, as.Date("2026-04-30"))) %>%
  rename(country=location) %>%
  left_join(ccodes, by=c("country", "year"))
check <- cw %>%
  filter(loc!=ccode | is.na(ccode)) #use 'loc'
rm(check)
cw <- cw %>%
  select(-ccode) %>%
  rename(ccode=loc) %>%
  select(-year) %>%
  distinct()

#now make it ccode/year/month
cw <- cw %>%
  mutate(start_date2 = as.Date(start_date2)) %>%
  mutate(ep_end_date = as.Date(ep_end_date)) %>%
  mutate(start=floor_date(start_date2, unit="month")) %>%
  mutate(end=floor_date(ep_end_date, unit="month")) %>%
  rowwise() %>%
  mutate(month_seq=list(seq.Date(start, end, by="month"))) %>%
  ungroup() %>%
  select(ccode, month_seq) %>%
  unnest(month_seq) %>%
  mutate(month_seq = month_seq %m+% months(1)) %>% #just lagged by 1 month
  mutate(year=year(month_seq)) %>%
  mutate(month=month(month_seq)) %>%
  mutate(cw=1) %>%
  set_variable_labels(cw = "ACD CW, t-1") %>%
  select(-month_seq) %>%
  distinct()

#merge into base
base_data <- base_data %>%
  left_join(cw, by=c("ccode", "year", "month")) %>%
  mutate(cw = ifelse(is.na(cw), 0, cw))
rm(cw)

#------------------------------------------------------------------------------------------------#
#Interstate conflicts; https://ucdp.uu.se/downloads/index.html#armedconflict
#------------------------------------------------------------------------------------------------#  

#bring in data
url <- "https://ucdp.uu.se/downloads/dyadic/ucdp-dyadic-251-xlsx.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
mid <- read_excel("data/Dyadic_v25_1.xlsx")
unlink("data", recursive=TRUE)
rm(url)  

df <- mid %>%
  filter(type_of_conflict==2) %>%
  select(start=start_date2, gwno_a, gwno_b, )






https://ucdp.uu.se/downloads/dyadic/ucdp-dyadic-251-rds.zip



https://ucdp.uu.se/downloads/#dyadic






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




















