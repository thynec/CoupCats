#------------------------------------------------------------------------------------------------#  
#Ccodes and Baseline Data
#------------------------------------------------------------------------------------------------#  

# Country codes (Thyne 2022). 
url <- "https://www.uky.edu/~clthyn2/replace_ccode_country.xls" # Bringing in ccodes to merge. 
destfile <- "replace_ccode_country.xls"
curl::curl_download(url, destfile)
ccodes <- read_excel(destfile)
rm(url, destfile)

#update so we have most recent month: 04/2026
base_data <- read_csv("https://www.uky.edu/~clthyn2/base_data.csv") # Reading in base data. 
update <- base_data %>%
  filter(year>=2023) %>%
  mutate(year=year+2)
base_data <- base_data %>%
  full_join(update, by=c("ccode", "year", "month", "country")) %>%
  arrange(ccode, year, month)
rm(update)

# Coup data (Powell & Thyne 2011). 
# Reading in data. 
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) 

# Cleaning up data. 
check <- coup_data %>%
  filter(ccode==lag(ccode) & year==lag(year) & month==lag(month))
#we have 6 cases of 2 coups in the same month:
#Haiti, 41, 04/1989; both failed
#Bolivia, 145, 05/1981; both failed
#Argentina, 160, 12/1975; both failed
#Sierra Leone, 451, 03/1967; second successful
#Togo, 461, 10/1991; both failed
#Sudan, 625, 12/1966; both failed
#Just need to make successful in Sierra Leone trump the failed one; all others are fine because both failed
table(coup_data$coup)
coup_data <- coup_data %>%
  mutate(coup=ifelse(ccode==451 & year==1967 & month==3, 2, coup))
table(coup_data$coup) #looks good; proceed...
rm(check)

coup_data <- coup_data %>% 
  select(-ccode_gw, -ccode_polity, -day, -version) %>%
  distinct() %>% #dropped the obs where 2 coups in same month, as above
  mutate(coup_attempt = 1) %>%
  mutate(coup_successful=ifelse(coup==2, 1, NA)) %>%
  mutate(coup_failed=ifelse(coup==1, 1, NA))
table(coup_data$coup_attempt)
table(coup_data$coup, coup_data$coup_attempt)
table(coup_data$coup, coup_data$coup_successful)
table(coup_data$coup, coup_data$coup_failed)
#after merge, should have...
#487 attempts
#246 successful
#241 failed
#everything above looks good; proceed...

# Merging into data set. 
coup_data <- coup_data %>%
  select(-country, -coup)
base_data <- base_data %>% 
  left_join(coup_data, by = c("year", "ccode", "month"))
base_data <- base_data %>%
  mutate(coup_attempt = ifelse(is.na(coup_attempt) & year>=1950, 0, as.numeric(coup_attempt))) %>%
  mutate(coup_successful = ifelse(is.na(coup_successful) & year>=1950, 0, as.numeric(coup_successful))) %>%
  mutate(coup_failed = ifelse(is.na(coup_failed) & year>=1950, 0, as.numeric(coup_failed))) 
table(base_data$coup_attempt) 
table(base_data$coup_successful) 
table(base_data$coup_failed) 
#we lost one successful coup because COW doesn't recognize Oman in 1970; we're good on this; proceed...
rm(coup_data) # Keeping things clean

# Set up non-coup months to deal with autocorrelation in the analyses
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(sequence=row_number())
base_data <- base_data %>%
  btscs('coup_attempt', 'year', 'ccode') %>%
  rename(pce=spell_time) %>%
  mutate(pce2=pce*pce) %>%
  mutate(pce3=pce*pce*pce) %>%
  set_variable_labels(
    pce="Months since last coup",
    pce2="Months^2",
    pce3="Months^3") %>%
  select(-sequence)

#Set up for successful coups; ignore failed coups
base_data <- base_data %>%
  select(-coup_failed)
base_data <- base_data %>%
  btscs('coup_successful', 'year', 'ccode') %>%
  rename(pce_succ=spell_time) %>%
  mutate(pce2_succ=pce_succ*pce_succ) %>%
  mutate(pce3_succ=pce_succ*pce_succ*pce_succ) %>%
  set_variable_labels(
    pce_succ="Months since last coup, succ",
    pce2_succ="Months^2, succ",
    pce3_succ="Months^3, succ") %>%
  ungroup()
base_data <- base_data %>%
  relocate(country, ccode, year, month, coup_attempt, pce, pce2, pce3)

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

# create onset month variable first
cw_onsets <- cw %>%
  mutate(start_date2 = as.Date(start_date2)) %>%
  mutate(onset_month = floor_date(start_date2, unit = "month")) %>%
  mutate(year = year(onset_month),
         month = month(onset_month),
         cw_onset = 1) %>%
  select(ccode, year, month, cw_onset) %>%
  distinct()

# now make ongoing cw country-year-month data
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
  mutate(year=year(month_seq)) %>%
  mutate(month=month(month_seq)) %>%
  mutate(cw=1) %>%
  set_variable_labels(cw = "ACD CW") %>%
  select(-month_seq) %>%
  distinct()

# merge onset onto ongoing CW data
cw <- cw %>%
  left_join(cw_onsets, by = c("ccode", "year", "month")) %>%
  mutate(cw_onset = ifelse(is.na(cw_onset), 0, cw_onset)) %>%
  set_variable_labels(
    cw = "ACD CW",
    cw_onset = "ACD CW onset month"
  )

#merge into base
base_data <- base_data %>%
  left_join(cw, by=c("ccode", "year", "month")) %>%
  mutate(cw = ifelse(is.na(cw), 0, cw)) %>%
  mutate(cw_onset = ifelse(is.na(cw_onset), 0, cw_onset))
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(sequence=row_number())
base_data <- base_data %>%
  btscs('cw', 'year', 'ccode') %>%
  rename(pce_cw=spell_time) %>%
  mutate(pce2_cw=pce_cw*pce_cw) %>%
  mutate(pce3_cw=pce_cw*pce_cw*pce_cw) %>%
  set_variable_labels(
    pce_cw="Months since last cw",
    pce2_cw="Months^2",
    pce3_cw="Months^3") %>%
  select(-sequence)
rm(cw, cw_onsets)

#------------------------------------------------------------------------------------------------#
#WB + ACLED splice for instability
#------------------------------------------------------------------------------------------------#  

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
  set_variable_labels(stability="from WB") %>%
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
  set_variable_labels(acledlz = "acled, log10, Z") %>%
  mutate(stability_WB=stability_WB*-1) %>%
  mutate(stability_OG=stability_OG*-1)
#go with acledlz if it exists; WB if not
base_data <- base_data %>%
  mutate(protests=stability_WB) %>%
  mutate(protests=ifelse(!is.na(acledlz), acledlz, protests)) %>%
  rename(acled_OG=acled) %>%
  set_variable_labels(acledl = "acled, t-1, log10") %>%
  set_variable_labels(acled_OG = "acled, t-1, original") %>%
  set_variable_labels(protests="acled+WB splice, t-1, Z")
base_data <- base_data %>%
  select(-stability_OG, -stability_WB, -acled_OG, -acledl, -acledlz)
