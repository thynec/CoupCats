#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building domestic political variables

#1. clear all
rm(list = ls())
#2. set working directory
#setwd("~/R/coupcats") # Set working file. 
#setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
#setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/teaching/1.coupcast/TEK_S26/git_2026.03.13") #clay at work
#3. install packages
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#------------------------------------------------------------------------------------------------#  
#bring in all vdem relevant data; clean it up
#------------------------------------------------------------------------------------------------#  
#Note: The V-Dem dataset does not cover some countries, namely: Andorra, Antigua and Barbuda, Bahamas, Belize, Brunei, Dominica, Federated States of Micronesia, Grenada, Kiribati, Liechtenstein, Marshall Islands, Monaco, Nauru, Palau, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines, Samoa, San Marino, Tonga, Tuvalu, and the Vatican.
vdem_og <- vdem
vdem <- vdem %>%
  subset(select = c(country_name, # Country. 
                    e_regionpol_6C, # Region. 
                    year, # Year. 
                    v2x_regime, # Regime type. 
                    v2x_polyarchy, # Regime type. 
                    v2x_execorr, # Executive corruption index.
                    v2x_jucon, # Judicial constraints on the executive index ordinal
                    v2x_corr, # Political corruption. 
                    v2cacamps, # Political polarization.
                    v2x_civlib, # Civil liberties. 
                    v2x_rule, # Rule of law. 
                    v2xcs_ccsi, # Core civil society index. 
                    v2x_genpp, # Women political participation index. 
                    v2x_gender, # Women political empowerment index. 
                    v2x_gencl, # Women civil liberties.
                    e_pelifeex, # Life expectancy. 
                    e_gdp, # GDP. 
                    e_gdppc, # GDPPC. 
                    e_miinflat, # Inflation rate. 
                    e_cow_exports, # Exports. 
                    e_cow_imports, # Imports. 
                    v2smgovshut, # Government Internet shut down in practice.
                    v2smgovfilprc, # Government  Internet filtering in practice, 
                    v2smfordom)) # Foreign governments dissemination of false information. 
'Assume these columns are clear of NAs unless stated otherwise.'

vdem <- vdem %>%
  rename(country = country_name,
         region = e_regionpol_6C,
         year = year, 
         regime = v2x_regime, 
         regime2 = v2x_polyarchy,
         exec_corr = v2x_execorr, 
         jud_const = v2x_jucon, 
         pol_corr = v2x_corr, 
         civ_lib = v2x_civlib, 
         pol_polar = v2cacamps, 
         law_rule = v2x_rule, # Ditto Timor-Leste. 
         civ_soc = v2xcs_ccsi, 
         wom_polpart = v2x_genpp, # Lots of issues... run: na_df <- vdem[is.na(vdem$wom_polpart), , drop = FALSE]
         women_polemp = v2x_gender, # Run: na_df <- vdem[is.na(vdem$v2x_gender), , drop = FALSE]
         wom_civlib = v2x_gencl, # No issues--could replace wom_polpart and women_polemp 
         life_exp = e_pelifeex, # Between 1800 to 2022; missing South Yemen, Republic of Vietnam (1950-75), Kosovo, German Democratic Republic, Palestine/Gaza, Somaliland, Hong Kong, Zanzibar 
         gdp = e_gdp, # Between 1789 to 2019... good for past data, might need something else for current data. 
         gdppc = e_gdppc, # Ditto GDP. 
         infla_rate = e_miinflat, # Ditto GDP. 
         exports = e_cow_exports, # Between 1870 to 2014.
         imports = e_cow_imports, # Between 1870 to 2014.
         int_shutdown = v2smgovshut, # Between 2000 to 2023... can likely code this to be 0 for previous years. 
         int_censor = v2smgovfilprc, # Between 2000 to 2023.
         forgov_misinfo = v2smfordom) %>% # Between 2000 to 2023.
  filter(year >= 1949)
vdem <- vdem %>% # Merging in ccodes. 
  left_join(ccodes, by = c("year", "country")) %>%
  filter(!is.na(ccode)) # Non-state actors. 

#------------------------------------------------------------------------------------------------#
# Leader age; regime age data; emailed from Powell to Thyne on 04/18/25
#------------------------------------------------------------------------------------------------#

tenure <- read_csv("https://github.com/thynec/CoupCats/raw/refs/heads/data/leader_tenure_csv.csv")
tenure <- tenure %>%
  mutate(drop = ifelse(ccode == lag(ccode) & year == lag(year) & month == lag(month), 1, 0)) %>%
  filter(drop!=1) #basically just lagged so outgoing leaders is counted in months where there was a transition; see Powell's email for a better way
tenure <- tenure %>%
  select(ccode, month, year, Leader_duration=month_counter, birthyear) %>%
  mutate(Leader_age=year-birthyear+1) %>%
  select(-birthyear)
base_data <- base_data %>%
  left_join(tenure, by=c("ccode", "year", "month"))
rm(tenure)

#------------------------------------------------------------------------------------------------#
#Regime type (v2x_regime); from Vdem
#------------------------------------------------------------------------------------------------#  

#Reading in data. Cleaning it up.
regime_type <- vdem %>%
  subset(select = c(country, year, regime)) %>%
  rename(regime_type = regime) %>%
  mutate(year=year+1) %>% #just lagged
  filter(year >= 1950,
         year < 2026)
regime_type <- regime_type %>% 
  left_join(ccodes, by = c("country", "year")) %>% # NAs resulting from state-like actors, not full states.  
  subset(select = -c(country))   %>% # To prevent future duplicated columns. 
  drop_na() %>%
  distinct() # No duplicates
label(regime_type$regime_type) <- "0 = Closed autocracy, 1 = Electoral autocracy, 2 = Electoral democracy, 3 = Liberal Democracy"

#Organizing variables for regression 
regime_type <- regime_type %>%
  mutate(
    closed_autocracy = ifelse(regime_type == 0, 1, 0),
    electoral_autocracy = ifelse(regime_type == 1, 1, 0),
    electoral_democracy = ifelse(regime_type == 2, 1, 0),
    liberal_democracy = ifelse(regime_type == 3, 1, 0)
  ) %>%
  set_variable_labels(
    closed_autocracy="from Vdem, t-1",
    electoral_autocracy="from Vdem, t-1",
    electoral_democracy="from Vdem, t-1",
    liberal_democracy="fromvdem, t-1"
  )
table(regime_type$regime_type, regime_type$closed_autocracy)
table(regime_type$regime_type, regime_type$electoral_autocracy)
table(regime_type$regime_type, regime_type$electoral_democracy)
table(regime_type$regime_type, regime_type$liberal_democracy)
#all above looks good
regime_type <- regime_type %>%
  select(-regime_type)

#Expand to create year=2026; assume it's the same as 2025
regime_type <- regime_type %>%
  mutate(expand=ifelse(year==2025, 2, 1)) %>%
  expandRows("expand", drop=FALSE) %>%
  arrange(ccode, year) %>%
  mutate(year=ifelse(year==2025 & lag(year)==2025 & ccode==lag(ccode), 2026, year)) %>%
  select(-expand) %>%
  mutate(month=12)

#Merging into data set. 
base_data <- base_data %>% 
  left_join(regime_type, by = c("ccode", "year", "month")) # Missing data simply is not updated by V-Dem, so I will not be dropping them. 
rm(regime_type)

#best option with these regime vars is linear interpolation for missing months (non-Decembers), so do that...

base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode, year) %>%
  fill(closed_autocracy, .direction = "updown") %>%
  fill(electoral_autocracy, .direction = "updown") %>%
  fill(electoral_democracy, .direction = "updown") %>%
  fill(liberal_democracy, .direction = "updown") %>%
  ungroup()

#------------------------------------------------------------------------------------------------#
#Regime type (v2x_polyarchy); from Vdem
#------------------------------------------------------------------------------------------------#  

vdem_regime2 <- vdem %>% 
  subset(select = c(country, ccode, year, regime2)) %>% 
  rename(polyarchy=regime2) %>%
  filter(!(country == "Kazakhstan" & year == 1990)) %>% # Kazakhstan became independent this year.
  filter(!(country == "Turkmenistan" & year == 1990)) %>% # Turkmenistan became independent this year. 
  subset(select = -c(country)) %>%
  mutate(year=year+1) %>% #just lagged
  mutate(polyarchy2=polyarchy*polyarchy) %>%
  set_variable_labels(
    polyarchy="v2x_polyarchy, vdem, t-1",
    polyarchy2="v2x_polyarchy^2, vdem, t-1") %>%
  mutate(month=12)

#merge to baseline
base_data <- base_data %>% 
  left_join(vdem_regime2, by = c("ccode", "year", "month")) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode, year) %>%
  fill(polyarchy, .direction="updown") %>%
  fill(polyarchy2, .direction="updown") %>%
  ungroup()
rm(vdem, vdem_regime2)

#------------------------------------------------------------------------------------------------#
#military regime; take from vdem
#------------------------------------------------------------------------------------------------#      

#bring in; clean
milit <- vdem_og %>%
  select(country=country_name, year, v2x_ex_military) %>%
  rename(milit=v2x_ex_military) %>%
  mutate(year=year+1) %>% #just lagged
  filter(year>1945) %>%
  left_join(ccodes, by = c("country", "year")) %>%
  set_variable_labels(milit="milit dimension index, vdem, t-1") 
check <- milit %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct() #all good
rm(check)

milit <- milit %>%
  select(-country) %>%
  mutate(month=12)
base_data <- base_data %>%
  left_join(milit, by=c("ccode", "year", "month")) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode, year) %>%
  fill(milit, .direction="updown")
rm(milit)

# #------------------------------------------------------------------------------------------------#
# #elections; from Vdem
# #------------------------------------------------------------------------------------------------#    

# #read in country-date version
# url <- "https://www.v-dem.net/media/datasets/V-Dem-CD-v14_csv_5jzTg6X.zip"
# download.file(url, "data.zip")
# unzip("data.zip", exdir="data")
# unlink("data.zip")
# df_og <- read_csv("data/V-Dem-CD-v14.csv")
# unlink("data", recursive=TRUE)
# rm(url)  

# #clean up data
# summary(df_og$year) #1789 through 2023 for missing
# df <- df_og %>%
#   filter(year>1945) %>%
#   select(
#     country_name, year, historical_date, 
#     v2eltype_0, v2eltype_1, v2eltype_2, v2eltype_3, v2eltype_4, v2eltype_5, v2eltype_6, v2eltype_7, v2eltype_8, v2eltype_9)
# df <- df %>%
#   filter(!is.na(v2eltype_0)) %>% # removing non-election data
#   mutate(election=1) %>%
#   mutate(legis_elec=ifelse(v2eltype_0==1 | v2eltype_1==1 | v2eltype_2==1 | v2eltype_3==1, 1, 0)) %>%
#   mutate(pres_elec=ifelse(v2eltype_6==1 | v2eltype_7==1, 1, 0)) 
# df <- df %>%
#   select(country_name, year, historical_date, election, legis_elec, pres_elec) %>%
#   mutate(month=month(historical_date)) %>%
#   select(country=country_name, year, month, election, legis_elec, pres_elec) %>%
#   left_join(ccodes, by=c("country", "year"))
# #collapse so we don't have duplicate ccode/year/months; then merge; one at a time
# df2 <- df %>%
#   group_by(ccode, year, month) %>%
#   dplyr::summarize(election = max(election, na.rm=TRUE)) 
# base_data <- base_data %>%
#   left_join(df2, by=c("ccode", "year", "month"))
# df2 <- df %>%
#   group_by(ccode, year, month) %>%
#   dplyr::summarize(legis_elec = max(legis_elec, na.rm=TRUE)) 
# base_data <- base_data %>%
#   left_join(df2, by=c("ccode", "year", "month"))
# df2 <- df %>%
#   group_by(ccode, year, month) %>%
#   dplyr::summarize(pres_elec = max(pres_elec, na.rm=TRUE)) 
# base_data <- base_data %>%
#   left_join(df2, by=c("ccode", "year", "month"))  
# rm(df, df_og, df2)
# base_data <- base_data %>%
#   set_variable_labels(
#     election="any election, vdem",
#     legis_elec="legis elec, vdem", 
#     pres_elec="presidential elec, vdem") %>%
#   ungroup()

# #set up vars that are 3 months before elections
# base_data <- base_data %>%
#   arrange(ccode, year, month) %>%
#   mutate(election=ifelse(is.na(election) & year<2024, 0, election)) %>%
#   mutate(legis_elec=ifelse(is.na(legis_elec) & year<2024, 0, legis_elec)) %>%
#   mutate(pres_elec=ifelse(is.na(pres_elec) & year<2024, 0, pres_elec)) 
# base_data <- base_data %>%
#   mutate(elec=ifelse(ccode==lead(ccode), lead(election), election)) %>%
#   mutate(elec2=ifelse(ccode==lead(ccode), lead(elec), election)) %>%
#   mutate(elec_lag=elec+elec2+election) %>%
#   set_variable_labels(elec_lag="1 if 1-3 mo B4 any elec, vdem") %>%
#   select(-elec, -elec2)
# base_data <- base_data %>%
#   mutate(elec=ifelse(ccode==lead(ccode), lead(legis_elec), legis_elec)) %>%
#   mutate(elec2=ifelse(ccode==lead(ccode), lead(elec), legis_elec)) %>%
#   mutate(legis_elec_lag=elec+elec2+legis_elec) %>%
#   set_variable_labels(legis_elec_lag="1 if 1-3 mo B4 legis elec, vdem") %>%
#   select(-elec, -elec2)  
# base_data <- base_data %>%
#   mutate(elec=ifelse(ccode==lead(ccode), lead(pres_elec), pres_elec)) %>%
#   mutate(elec2=ifelse(ccode==lead(ccode), lead(elec), pres_elec)) %>%
#   mutate(pres_elec_lag=elec+elec2+pres_elec) %>%
#   set_variable_labels(pres_elec_lag="1 if 1-3 mo B4 pres elec, vdem") %>%
#   select(-elec, -elec2)  
# #set up vars that are 3 months after elections
# base_data <- base_data %>%
#   mutate(lead1=ifelse(ccode==lag(ccode), lag(election), election)) %>%
#   mutate(lead2=ifelse(ccode==lag(ccode), lag(lead1), election)) %>%
#   mutate(elec_lead=election+lead1+lead2) %>%
#   set_variable_labels(elec_lead="1 if 1-3 mo after any elec, vdem") %>%
#   select(-lead1, -lead2, -election)
# base_data <- base_data %>%
#   mutate(lead1=ifelse(ccode==lag(ccode), lag(legis_elec), legis_elec)) %>%
#   mutate(lead2=ifelse(ccode==lag(ccode), lag(lead1), legis_elec)) %>%
#   mutate(legis_elec_lead=legis_elec+lead1+lead2) %>%
#   set_variable_labels(legis_elec_lead="1 if 1-3 mo after legis elec, vdem") %>%
#   select(-lead1, -lead2, -legis_elec)  
# base_data <- base_data %>%
#   mutate(lead1=ifelse(ccode==lag(ccode), lag(pres_elec), pres_elec)) %>%
#   mutate(lead2=ifelse(ccode==lag(ccode), lag(lead1), pres_elec)) %>%
#   mutate(pres_elec_lead=pres_elec+lead1+lead2) %>%
#   set_variable_labels(pres_elec_lead="1 if 1-3 mo after pres elec, vdem") %>%
#   select(-lead1, -lead2, -pres_elec)  

#------------------------------------------------------------------------------------------------#
# election data; REIGN; 4/7/26
#------------------------------------------------------------------------------------------------#
install.packages("googlesheets4") # Reads Google Sheets files
library(googlesheets4) # Reads Google Sheets files
election <- read_sheet("1zYI_kqONAyMW1rehC-fkL7xIeqkbphkBgjc1amfrbfs")
# the googlesheets4 package requires that you log in to a google account when accessing data; would be better to import this as a csv

election_events <- election %>%
  rename(year = elec_year, month = elec_month) %>%
  filter(year > 1949 & year <= 2026) %>%
  mutate(monthnum = (year - 1950) * 12 + month) %>%
  select(ccode, elecnum, sequence, event, year, month, monthnum, victory)

election_held <- election_events %>%
  filter(event %in% c("Vote, As Scheduled", "Vote, Early", "Deadline Met")) %>%
  mutate(election = 1, 
         victory = case_when(victory == "Y" ~ 1, victory == "N" ~ 0, TRUE ~ NA_integer_)) %>%
  group_by(ccode, year, month) %>%
  dplyr::summarize(
    election = max(election, na.rm = TRUE),
    victory  = max(victory,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(victory = ifelse(is.infinite(victory), NA_integer_, victory))

# anticipation
vote_events <- election_events %>%
  filter(event %in% c("Vote, As Scheduled", "Vote, Early", "Deadline Met"))
anticipation_event_types <- c("Announced", "Announcement Assumed", "Upcoming, Scheduled", "Upcoming, Deadline")
first_announce <- election_events %>%
  filter(event %in% anticipation_event_types) %>%
  group_by(elecnum) %>%
  dplyr::summarize(first_announce_monthnum = min(monthnum, na.rm = TRUE), .groups = "drop")
vote_windows <- vote_events %>%
  left_join(first_announce, by = "elecnum") %>%
  mutate(
    anticipate_start = pmax(
      monthnum - 7,
      coalesce(first_announce_monthnum, monthnum - 7)
    ),
    anticipate_start = pmin(anticipate_start, monthnum - 1),
    window_length = monthnum - anticipate_start
  ) %>%
  filter(window_length > 0) %>%
  select(ccode, vote_monthnum = monthnum, window_length)
anticipation_panel <- vote_windows %>%
  rowwise() %>%
  mutate(offset = list(seq(1, window_length))) %>%
  unnest(offset) %>%
  ungroup() %>%
  mutate(
    monthnum = vote_monthnum - offset,
    anticipation = 1 / (offset + 1)
  ) %>%
  group_by(ccode, monthnum) %>%
  dplyr::summarize(anticipation = max(anticipation, na.rm = TRUE),
                   .groups = "drop") %>%
  mutate(
    year = 1950 + (monthnum - 1) %/% 12,
    month = ((monthnum - 1) %% 12) + 1
  ) %>%
  select(ccode, year, month, anticipation)

# merge back in
base_data <- base_data %>%
  left_join(election_held, by = c("ccode", "year", "month")) %>%
  left_join(anticipation_panel, by = c("ccode", "year", "month")) %>%
  mutate(
    election = replace_na(election, 0),
    anticipation = replace_na(anticipation, 0)
    # victory stays NA when no election occurred
  )

base_data <- base_data %>%
  set_variable_labels(
    election = "1 if election held",
    victory = "1 if incumbent won election",
    anticipation = "upcoming election"
  )

rm(election, election_events, election_held, vote_events, first_announce,
   vote_windows, anticipation_panel, anticipation_event_types)

#------------------------------------------------------------------------------------------------#
#milreg; emailed by Powell on 03/18/25; REIGN with updates
#------------------------------------------------------------------------------------------------#  

milreg <- read_csv("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/data/coupcats_military_regime.csv")
milreg <- milreg %>%
  rename(country_milreg=country) 

milreg <- milreg %>% 
  select(-reign_type) %>%
  mutate(year=year+1) %>% #just lagged
  distinct()
#originally filled all n/as as 0; now making assuption that milreg stays if 1 in march 2026 and then filling all remaining n/a with 0
base_data <- base_data %>%
  left_join(milreg, by = c("ccode", "year", "month"))
still_active <- milreg %>%
  filter(year == 2026, month == 3, milreg == 1) %>%
  pull(ccode)
base_data <- base_data %>%
  mutate(milreg = ifelse(ccode %in% still_active & year == 2026 & month == 4, 1, milreg)) 
base_data <- base_data %>%
  mutate(milreg = replace_na(milreg, 0))
rm(milreg)
base_data <- base_data %>%
  set_variable_labels(milreg="milreg from reign; powell updates") %>%
  select(-country_milreg)

#------------------------------------------------------------------------------------------------#
# Gender data (V-Dem) 
#------------------------------------------------------------------------------------------------#

gender_data <- vdem %>%
  subset(select = c(country_name, # Country. 
                    e_regionpol_6C, # Region. 
                    year, # Year. 
                    v2x_genpp, # Women political participation index. 
                    v2x_gender, # Women political empowerment index. 
                    v2x_gencl)) %>% # Women civil liberties.
  rename(country = country_name,
         region = e_regionpol_6C,
         year = year, 
         wom_polpart = v2x_genpp, 
         women_polemp = v2x_gender, 
         wom_civlib = v2x_gencl) %>% 
  mutate(year=year+1) %>% # Just lagged. 
  filter(year >= 1950)

# Merging in ccodes. 
gender_data <- gender_data %>% 
  left_join(ccodes, by = c("year", "country")) %>%
  filter(!is.na(ccode)) # Non-state actors. 

# Merging data. 
gender_data <- gender_data %>% 
  select(-country) %>% 
  mutate(month=12)
base_data <- base_data %>% 
  left_join(gender_data, by = c("ccode", "year", "month")) %>%
  mutate(wom_polpart_OG=wom_polpart) %>%
  mutate(women_polemp_OG=women_polemp) %>%
  mutate(wom_civlib_OG=wom_civlib) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(time_id = year + (month - 1) / 12) %>%
  mutate(wom_polpart=na.approx(wom_polpart, x=time_id, na.rm=FALSE, rule=2)) %>%
  mutate(women_polemp=na.approx(women_polemp, x=time_id, na.rm=FALSE, rule=2)) %>%
  mutate(wom_civlib=na.approx(wom_civlib, x=time_id, na.rm=FALSE, rule=2)) %>%
  select(-time_id)
rm(gender_data) 

#------------------------------------------------------------------------------------------------#
# military regime proportion over last 15 years; from milreg
#------------------------------------------------------------------------------------------------# 
#leah - fixed grouping issue by ccode; code produces N/As for years 1950-1965 because it doesn't have a full 15 years to compute proprtion. leaving N/As for now and we can discuss if a fill method should be used

sorted_base_data <- base_data %>%
  arrange(country, year, month)

base_data_prop <- sorted_base_data %>%
  group_by(ccode) %>%
  mutate(
    milreg_prop = rollapplyr(
      milreg, 
      width = 180, 
      FUN = function(x) mean(x==1, na.rm = TRUE), 
      fill = NA, 
      align = "right")
  )
base_data_prop <- base_data_prop %>%
  select(ccode, year, month, milreg_prop)

base_data <- left_join(base_data, base_data_prop,
                       by = c("ccode", "month", "year"))
rm(base_data_prop, sorted_base_data)

#---------------------------------------------------------------------------------------------#
#   Number of leaders over 5 and 10 years, Proportion of leaders in past 5 years with milit background (Reign)
#---------------------------------------------------------------------------------------------#
#leah- looked good; a few minor changes all runs smooth now

# import data and convert to data table
url <- "https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/data/leaderlist_2026_02_10.csv"
leader_data <- as.data.table(
  read_csv(url)
)
rm(url)

# Sort + create dates
setorder(leader_data, ccode, syear, smonth, sdate)
leader_data[, sdate := as.integer(sdate)]
leader_data[, sdate_full := make_date(syear, smonth, sdate)]
leader_data[, edate_full := shift(sdate_full, type = "lead"), by = ccode]
leader_data <- leader_data %>% # current leaders with na end dates set to future so they are included
  mutate(edate_full = replace_na(edate_full, as.Date("2100-01-01")))  

# make base data a data table and fix month/year issue
setDT(base_data)
setnames(base_data, c("year", "month"), c("Year", "Month"))
base_data[, date := make_date(Year, Month, 1)]

# Create windows
base_data[, `:=`(
  window5   = date %m-% years(5),
  window10  = date %m-% years(10),
  windowEnd = date %m-% months(1)
)]

# Set keys for fast joins
detach("package:data.table", unload = TRUE)
library(data.table)
setDT(leader_data)
setDT(base_data)
setkey(leader_data, ccode, sdate_full, edate_full)
setkey(base_data, ccode)

# ---- 5-year window ----
leaders_5yr <- leader_data[
  base_data,
  on = .(ccode,
         sdate_full <= windowEnd,   # include leaders who started before the window ends
         edate_full >= window5),    # and who ended after the window begins
  allow.cartesian = TRUE,
  .(numleaders_5yr = uniqueN(leader),
    prop_milit_career = {
      unique_leaders <- unique(.SD[, .(leader, militarycareer)])
      mean(unique_leaders$militarycareer == 1, na.rm = TRUE)
    },
    date = i.date),  
  by = .EACHI
]

# ---- 10-year window ----
leaders_10yr <- leader_data[
  base_data,
  on = .(ccode,
         sdate_full <= windowEnd,
         edate_full >= window10),
  allow.cartesian = TRUE,
  .(numleaders_10yr = uniqueN(leader),
    date = i.date),
  by = .EACHI
]

# Merge results back
both_leader <- leaders_5yr[leaders_10yr, on = .(ccode, date)]
base_data <- base_data[both_leader, on = .(ccode, date)]

#a few N/As in the prop_milit_career (1.16%) because we do not have data in reign; overwise looks good

# Clean up
rm(leader_data, both_leader, leaders_10yr, leaders_5yr)
base_data <- base_data %>% select(-date, -window5, -window10, -windowEnd, -edate_full, -i.edate_full, -sdate_full, -i.sdate_full)
setnames(base_data, c("Year", "Month"), c("year", "month"))
rm(vdem_og)
base_data <- base_data %>%
  select(-region)

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.a.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################

###############################################################################################
#Below is stuff we probably won't use but keeping it here just in case...
###############################################################################################

# -------------------------- Political Data ------------------------------ #
#
## 1. Perceptions of Corruption (Transparency International, Corruption Perceptions Index). 
## 1.1. Reading in data. 
#wdi_data <- WDI(indicator = c("CC.EST", "CC.PER.RNK"), country = "all", start = 1996, end = 2023, extra = TRUE) # Data are reported every two years--need to figure out a way to transform. 
#
## 1.2. Cleaning up data. 
#wdi_data <- wdi_data %>%
#  mutate(year=year+1) %>% #just lagged by adding a year
#  subset(select = c(country, year, CC.EST, CC.PER.RNK, region)) %>% 
#  rename(corruption_est = CC.EST, 
#         corruption_rank = CC.PER.RNK,
#         wdi_region = region) %>%
#  set_variable_labels(
#    corruption_est="Control of Corruption: Estimate; from WDI; t-1",
#    corruption_rank="Control of Corruption: Percentile Rank; from WDI; t-1",
#    wdi_region="regions from WDI"
#  )
#wdi_data <- wdi_data %>%
#  left_join(ccodes, by = c("year", "country")) 
#check <- wdi_data %>%
#  filter(if_any(everything(), is.na)) 
#table(check$country)
#table(check$year)
##check looks fine; missing 3 in normal places due to temporal coverage; other missings are due to small countries or regions
#rm(check) 
#
## 1.3. Merging into data set. Do this twice. First for corruption by ccode/year; then for region by ccode (region doesn't vary by time)
#wdi1 <- wdi_data %>%
#  select(-country, -wdi_region)
#base_data <- base_data %>% 
#  left_join(wdi1, by = c("year", "ccode")) # NAs are unreported years. 
#wdi2 <- wdi_data %>%
#  select(ccode, wdi_region) %>%
#  distinct()
#base_data <- base_data %>%
#  left_join(wdi2, by=c("ccode"))
#rm(wdi_data, wdi1, wdi2)
#
##Add regional dummies
#table(base_data$wdi_region)
#base_data <- base_data %>%
#  mutate(e_asia_pacific=ifelse(wdi_region=="East Asia & Pacific", 1, 0)) %>%
#  mutate(euro_cent_asia=ifelse(wdi_region=="Europe & Central Asia", 1, 0)) %>%
#  mutate(LA_carrib=ifelse(wdi_region=="Latin America & Caribbean", 1, 0)) %>%
#  mutate(MENA=ifelse(wdi_region=="Middle East & North Africa", 1, 0)) %>%
#  mutate(N_america=ifelse(wdi_region=="North America", 1, 0)) %>%
#  mutate(S_asia=ifelse(wdi_region=="South Asia", 1, 0)) %>%
#  mutate(Sub_africa=ifelse(wdi_region=="Sub-Saharan Africa", 1, 0)) %>%
#  set_variable_labels(
#    e_asia_pacific="East Asia & Pacific",
#    euro_cent_asia="Europe & Central Asia",
#    LA_carrib="Latin America & Caribbean",
#    MENA="Middle East & North Africa",
#    N_america="North America",
#    S_asia="South Asia",
#    Sub_africa="Sub-Saharan Africa") %>%
#  select(-wdi_region)
#
#
#
#
#
## 2. Population data (World Bank Data Group 2024). 
## 2.1. Reading in data. 
#url <- "https://extdataportal.worldbank.org/content/dam/sites/data/gender-data/data/data-gen/zip/indicator/population-number.zip"
#zip_file <- "population-data.zip"
#download.file(url, zip_file, mode = "wb")
#unzip(zip_file, exdir = "unzipped_data") 
#world_bank <- read.csv("unzipped_data/Population (number).csv") # WB is missing data for 2024. This will update automatically after they add it. 
#rm(url, zip_file)
#unlink("population-data.zip")
#unlink("unzipped_data", recursive=TRUE)
#
## 2.2. Cleaning up data. 
#pop <- world_bank %>%
#  filter(Indicator.Name == 'Population, total') %>%
#  subset(select = c(Country.Name, Year, Value)) %>%
#  rename(country = Country.Name,
#         year = Year,
#         population = Value) %>% 
#  left_join(ccodes, by = c("year", "country")) 
#check <- pop %>%
#  filter(if_any(everything(), is.na))
#table(check$country) #all regions or tiny countries; we're fine to drop NAs
#pop <- pop %>%
#  drop_na() %>%
#  select(-country)
#rm(check)
#check <- pop %>%
#  distinct() #no duplicates in pop data
#check <- base_data %>%
#  distinct() #no duplicates in base data
#rm(check)
#pop <- pop %>%
#  mutate(year=year+1)  %>%
#  mutate(pop=log(population)) %>%
#  set_variable_labels(
#    pop="Total pop, WDI, log, t-1"
#  ) %>%
#  select(-population)
#
## 2.3. Merging into data set. 
#base_data <- base_data %>% 
#  ungroup() %>%
#  left_join(pop, by = c("ccode", "year"))
#rm(pop)
#check <- base_data %>%
#  filter(is.na(pop))
#histogram(check$year) #looks fine but should be able to get pop for full sample from elsewhere
#rm(check)
#rm(world_bank)
#
## 4. Median age data (World Bank Data Group 2024)
## 4.1. Reading in data. 
#url <- "https://ourworldindata.org/grapher/median-age.csv?v=1&csvType=full&useColumnShortNames=true"
#median_age <- read_csv(url)
#rm(url)
#
## 4.2. Cleaning up data. 
#median_age <- median_age %>%
#  subset(select = c(Entity, Year, median_age__sex_all__age_all__variant_estimates, median_age__sex_all__age_all__variant_medium)) %>%
#  rename(country = Entity,
#         year = Year,
#         popln_median = median_age__sex_all__age_all__variant_estimates,
#         popln_median_est = median_age__sex_all__age_all__variant_medium) %>%
#  mutate(median_age = ifelse(is.na(popln_median), popln_median_est, popln_median)) %>%
#  subset(select = -c(popln_median, popln_median_est)) %>% 
#  filter(year <= 2024)
#median_age <- median_age %>%
#  mutate(year=year+1) %>% #just lagged
#  left_join(ccodes, by = c("country", "year")) 
#check <- median_age %>%
#  filter(is.na(ccode))
#table(check$country) #fine; all regions or very small countries
#rm(check)
#median_age <- median_age %>%
#  drop_na() %>% # NAs from non-country rows. 
#  distinct() %>% # Random duplicates. 
#  select(-country)
#
## 4.3. Merging into data set. 
#base_data <- base_data %>% 
#  left_join(median_age, by = c("ccode", "year")) 
#rm(median_age)
#
#
## -------------------------- V-Dem Data ------------------------------ #
#
## V-Dem data. 
## Reading in data. 
#vdem <- vdem 
#
#'The V-Dem dataset does not cover some countries, namely: 
#  Andorra, Antigua and Barbuda, Bahamas, Belize, Brunei, Dominica, 
#  Federated States of Micronesia, Grenada, Kiribati, Liechtenstein, 
#  Marshall Islands, Monaco, Nauru, Palau, Saint Kitts and Nevis, 
#  Saint Lucia, Saint Vincent and the Grenadines, Samoa, San Marino, 
#  Tonga, Tuvalu, and the Vatican.'
#
## Cleaning up data. 
#vdem <- vdem %>%
#  subset(select = c(country_name, # Country. 
#                    e_regionpol_6C, # Region. 
#                    year, # Year. 
#                    v2x_regime, # Regime type. 
#                    v2x_polyarchy, # Regime type. 
#                    v2x_execorr, # Executive corruption index.
#                    v2x_jucon, # Judicial constraints on the executive index ordinal
#                    v2x_corr, # Political corruption. 
#                    v2cacamps, # Political polarization.
#                    v2x_civlib, # Civil liberties. 
#                    v2x_rule, # Rule of law. 
#                    v2xcs_ccsi, # Core civil society index. 
#                    v2x_genpp, # Women political participation index. 
#                    v2x_gender, # Women political empowerment index. 
#                    v2x_gencl, # Women civil liberties.
#                    e_pelifeex, # Life expectancy. 
#                    e_gdp, # GDP. 
#                    e_gdppc, # GDPPC. 
#                    e_miinflat, # Inflation rate. 
#                    e_cow_exports, # Exports. 
#                    e_cow_imports, # Imports. 
#                    v2smgovshut, # Government Internet shut down in practice.
#                    v2smgovfilprc, # Government  Internet filtering in practice, 
#                    v2smfordom)) # Foreign governments dissemination of false information. 
#
#'Assume these columns are clear of NAs unless stated otherwise.'
#
#vdem <- vdem %>%
#  rename(country = country_name,
#         region = e_regionpol_6C,
#         year = year, 
#         regime = v2x_regime, 
#         regime2 = v2x_polyarchy,
#         exec_corr = v2x_execorr, 
#         jud_const = v2x_jucon, 
#         pol_corr = v2x_corr, 
#         civ_lib = v2x_civlib, 
#         pol_polar = v2cacamps, 
#         law_rule = v2x_rule, # Ditto Timor-Leste. 
#         civ_soc = v2xcs_ccsi, 
#         wom_polpart = v2x_genpp, # Lots of issues... run: na_df <- vdem[is.na(vdem$wom_polpart), , drop = FALSE]
#         women_polemp = v2x_gender, # Run: na_df <- vdem[is.na(vdem$v2x_gender), , drop = FALSE]
#         wom_civlib = v2x_gencl, # No issues--could replace wom_polpart and women_polemp 
#         life_exp = e_pelifeex, # Between 1800 to 2022; missing South Yemen, Republic of Vietnam (1950-75), Kosovo, German Democratic Republic, Palestine/Gaza, Somaliland, Hong Kong, Zanzibar 
#         gdp = e_gdp, # Between 1789 to 2019... good for past data, might need something else for current data. 
#         gdppc = e_gdppc, # Ditto GDP. 
#         infla_rate = e_miinflat, # Ditto GDP. 
#         exports = e_cow_exports, # Between 1870 to 2014.
#         imports = e_cow_imports, # Between 1870 to 2014.
#         int_shutdown = v2smgovshut, # Between 2000 to 2023... can likely code this to be 0 for previous years. 
#         int_censor = v2smgovfilprc, # Between 2000 to 2023.
#         forgov_misinfo = v2smfordom) %>% # Between 2000 to 2023.
#  filter(year >= 1950)
#vdem <- vdem %>% # Merging in ccodes. 
#  left_join(ccodes, by = c("year", "country")) %>%
#  filter(!is.na(ccode)) # Non-state actors. 





## 3. Executive corruption index (v2x_execorr).
#vdem_execorr <- vdem %>% 
#  subset(select = c(country, ccode, year, exec_corr)) %>%
#  filter(!(ccode == "860" & year < 2001)) %>% # Timor-Leste gained independence in 2000. 
#  subset(select = -c(country))
#base_data <- base_data %>% 
#  left_join(vdem_execorr, by = c("ccode", "year")) 
#rm(vdem_execorr)
#
## 4. Judicial constraints on the executive index (v2x_jucon)
#vdem_judconst <- vdem %>% 
#  subset(select = c(country, ccode, year, jud_const)) %>%
#  filter(!(ccode == "860" & year < 2001)) %>% 
#  subset(select = -c(country))
#base_data <- base_data %>% 
#  left_join(vdem_judconst, by = c("ccode", "year")) %>%
#  filter(!(ccode == "692" & year < 2002)) # Current government of Bahrain established in 2002.
#rm(vdem_judconst)
#
## 5. Political corruption (v2x_corr). 
#vdem_polcorr <- vdem %>%
#  subset(select = c(country, ccode, year, pol_corr)) %>%
#  filter(!(ccode == "860" & year < 2001)) %>% 
#  filter(!(ccode == "692" & year < 2002)) 
## We need to decide what to do with Bahrain 2002-2004... 
#
#rm(vdem_polcorr)
#
## 6. Polarization of society (v2smpolsoc). 
#vdem_polpolar <- vdem %>%
#  subset(select = c(country, ccode, year, pol_polar)) 
## No South Yemen (1950-90), Republic of Vietnam (1950-75), Afghanistan (1950-1991), Papua New Guinea (2021-23), Comoros (2023), Iceland (1950-1991)
#
#rm(vdem_polpolar)
#
## 7. Civil liberties (v2x_civlib).
#vdem_civlib <- vdem %>% 
#  subset(select = c(country, ccode, year, civ_lib)) %>% 
#  subset(select = -c(country))
#base_data <- base_data %>% 
#  left_join(vdem_civlib, by = c("ccode", "year"))  
#rm(vdem_civlib)

# 3. Age population data (World Bank Data Group 2024).
# 3.1. Reading in data. 
#age_popln <- world_bank 
#%>%

# 3.2. Cleaning up data. 
#  filter(Indicator.Name %in% c('Population ages 0-14, total',
#                               'Population ages 15-64, total',
#                               'Population ages 65 and above, total')) %>%
#  subset(select = c(Country.Name, Year, Value, Disaggregation)) %>%
#  rename(country = Country.Name,
#         year = Year,
#         age_tot = Value,
#         type = Disaggregation)
#age_popln <- age_popln %>%
#  left_join(ccodes, by = c("country", "year"), relationship = "many-to-many") %>% 
#  drop_na() %>% # NAs from non-country rows. 
#  distinct() # Random duplicates. 
#age_popln <- age_popln %>%
#  pivot_wider(names_from = type, values_from = age_tot, values_fn = first) %>%
#  rename(age0_14 = `total, 0-14`, 
#         age15_64 = `total, 15-64`,
#         age65plus = `total, 65+`) 

# 3.3. Merging into data set. 
#emma_data <- emma_data %>% # No 2024 data. 
#  left_join(age_popln, by = c("country", "year", "ccode"), relationship = "many-to-many") %>%
#  distinct() # Random duplicates. 
#rm(world_bank, age_popln)








# 5. Age expectancy data. 
# 5.1. Reading in data. 
#url <- "https://srhdpeuwpubsa.blob.core.windows.net/whdh/DATADOT/INDICATOR/C64284D_ALL_LATEST.csv"
#age_expectancy <- read_csv(url)
#rm(url)

# 5.2. Cleaning up data. 
#age_expectancy <- age_expectancy %>%
#  subset(select = c(DIM_TIME, GEO_NAME_SHORT, DIM_SEX, AMOUNT_N)) %>%
#  filter(DIM_SEX == 'TOTAL') %>%
#  rename(country = GEO_NAME_SHORT, 
#         year = DIM_TIME, 
#         age_expectancy = AMOUNT_N) %>%
#  subset(select = -c(DIM_SEX))
#age_expectancy <- age_expectancy %>%
#  left_join(ccodes, by = c("country", "year")) %>% # Missing 'Côte d'Ivoire', 'Türkiye', 'Zambia', 'Zimbabwe'
#  drop_na() %>% # NAs from non-country rows. 
#  distinct() # Random duplicates from Yemen. 

# 5.3. Merging into data set. 
#emma_data <- emma_data %>% 
#  left_join(age_expectancy, by = c("country", "year", "ccode"), relationship = "many-to-many")
#rm(age_expectancy)  

