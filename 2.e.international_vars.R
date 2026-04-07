#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building international-related variables

#1. clear all
rm(list = ls())
#2. set working directory
#setwd("~/R/coupcats") # Set working file. 
#setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/teaching/1.coupcast/TEK_S26/git_2026.03.13") #clay at work
#3. install packages
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

# -------------------------- Int Signals ----------------------------- #

# 1. WEIS DATA
# 1.1. Getting the data
url <- "https://github.com/thynec/CoupCats/raw/refs/heads/data/weis.fromlai.1966-93.dta" #bringing in the data
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

weis <- weis %>% 
  dplyr::select(year,target, month, z_variable)

# 2. COPDAB DATA
# 2.1. Getting the data
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

copdab <-  copdab %>% 
  dplyr::select(year,target, month, z_variable)

# 3. Merging both
int_signals <- weis %>%
  full_join(copdab, by=c("target", "year", "month", "z_variable"))

int_signals <-int_signals %>% 
  group_by(year, target, month) %>% 
  mutate(z= mean(z_variable)) %>%
  select(-z_variable) %>%
  arrange(target, year, month) %>%
  distinct() %>%
  ungroup() %>%
  rename(ccode=target)
#lag by 1 month before merge
int_signals <- int_signals %>%
  mutate(
    date = ymd(paste(year, month, "01")),
    date = date %m+% months(1),
    year = year(date),
    month = month(date)
)

base_data <- base_data %>% 
  left_join(int_signals, by = c("ccode", "year", "month")) %>%
  mutate(z=ifelse(year>=1950 & year<=1992 & is.na(z), 0, z)) %>%
  select(-date) %>%
  rename(signal=z) %>%
  set_variable_labels(signal="copdab+weis, t-1")

rm(copdab, weis, int_signals)

#-----------------------------------------------------------------# 
#KOF Globalization 
#-----------------------------------------------------------------#

kof <- read_csv("https://github.com/catalinahix06-star/CoupCats/raw/refs/heads/main/KOFGI_2025_public(Sheet1).csv")

kof <- kof %>%
  select(KOFTrGIdf, #trade globalization, de facto
         KOFPoGIdj, #political globalization, de jure
         KOFCuGIdf, #gender parity 
         KOFIpGIdf, #interpersonal globalization de facto
         country,
         code,
         year) %>%
  mutate(month = list(1:12)) %>%  #expand to monthly 
  unnest(month) %>%
  mutate(across(
    c(KOFTrGIdf, 
      KOFPoGIdj, 
      KOFCuGIdf, 
      KOFIpGIdf), 
    ~ ifelse(month == 12, .x, NA)
  )) %>% 
  mutate(year=year+1) %>% #lagging
  mutate(across(c
                (KOFTrGIdf, 
                  KOFPoGIdj, 
                  KOFCuGIdf, 
                  KOFIpGIdf),
                ~ .x / 100))  #need to make the percentages back to regular numbers 
kof <- kof %>%
  mutate(country=ifelse(country=="Congo, Rep", "Congo", country)) %>%
  mutate(country=ifelse(country=="Egypt, Arab Rep", "Egypt", country)) %>%
  mutate(country=ifelse(country=="Iran, Islamic Rep", "Iran", country)) %>%
  mutate(country=ifelse(country=="Korea, Rep", "South Korea", country)) %>%
  mutate(country=ifelse(code=="PRK", "North Korea", country)) %>%
  mutate(country=ifelse(country=="Yemen, Rep", "Yemen", country)) %>%
  mutate(country=ifelse(country=="Congo, Dem Rep", "Democratic Republic of Congo", country)) %>%
  left_join(ccodes, by=c("country", "year"))
check <- kof %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct()
rm(check)
kof <- kof %>%
  filter(!is.na(ccode)) %>%
  select(-code) %>%
  rename(trade_glob_OG=KOFTrGIdf) %>%
  rename(pol_glob_OG=KOFPoGIdj) %>%
  rename(gender_parity_OG=KOFCuGIdf) %>%
  rename(interpersonal_glob_OG=KOFIpGIdf) %>%
  set_variable_labels(trade_glob_OG="trade globalization, de facto, t-1, KOF") %>%
  set_variable_labels(pol_glob_OG="political globalization, de jure, t-1, KOF") %>%
  set_variable_labels(gender_parity_OG="gender parity, t-1, KOF") %>%
  set_variable_labels(interpersonal_glob_OG="interpersonal globalization de facto, t-1, KOF") %>%
  select(-country)
base_data <- base_data %>%
  left_join(kof, by = c("ccode", "year", "month"))  %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(time_id = year + (month - 1) / 12) %>%
  mutate(trade_glob=trade_glob_OG) %>%
  mutate(trade_glob = if_else(
    year >= 1971 & (year < 2026 | (year == 2026 & month <= 4)),
    na.approx(trade_glob, x = time_id, na.rm = FALSE, rule = 2),
    trade_glob
  )) %>%
  mutate(pol_glob=pol_glob_OG) %>%
  mutate(pol_glob = if_else(
    year >= 1971 & (year < 2026 | (year == 2026 & month <= 4)),
    na.approx(pol_glob, x = time_id, na.rm = FALSE, rule = 2),
    pol_glob
  )) %>%
  mutate(gender_parity=gender_parity_OG) %>%
  mutate(gender_parity = if_else(
    year >= 1971 & (year < 2026 | (year == 2026 & month <= 4)),
    na.approx(gender_parity, x = time_id, na.rm = FALSE, rule = 2),
    gender_parity
  )) %>%
  mutate(interpersonal_glob=interpersonal_glob_OG) %>%
  mutate(interpersonal_glob = if_else(
    year >= 1971 & (year < 2026 | (year == 2026 & month <= 4)),
    na.approx(interpersonal_glob, x = time_id, na.rm = FALSE, rule = 2),
    interpersonal_glob
  )) 

rm(kof)  

# ------------------- FDI (World Bank) ------------------- #
# Bring in FDI (World Bank). 
fdi <- WDI(indicator = "BX.KLT.DINV.CD.WD", start = 1960, end = 2026, extra = TRUE)

# Cleaning up FDI. 
fdi <- fdi %>% 
  select(country, year, BX.KLT.DINV.CD.WD) %>%
  rename(fdi = BX.KLT.DINV.CD.WD) %>%
  mutate(country=ifelse(country=="Turkiye", "Turkey", country)) %>%
  mutate(country=ifelse(country=="Somalia, Fed. Rep.", "Somalia", country)) %>%
  left_join(ccodes, by =  c("country", "year")) 
check <- fdi %>%
  filter(is.na(ccode)) %>%
  select(country) %>% 
  distinct() #all good
rm(check)
fdi <- fdi %>%
  select(-country) %>%
  mutate(year=year+1) %>% #just lagged, so we have 1961-2026
  mutate(month=12) %>%
  rename(fdi_OG=fdi)

# Merging into base data. 
base_data <- base_data %>%
  left_join(fdi, by = c("ccode", "year", "month")) 

base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(time_id = year + (month - 1) / 12) %>%
  mutate(fdi=fdi_OG) %>%
  mutate(fdi = if_else(
      year >= 1961 & (year < 2026 | (year == 2026 & month <= 4)),
      na.approx(fdi, x = time_id, na.rm = FALSE, rule = 2),
      fdi
    )
  ) %>%
  set_variable_labels(fdi="fdi, t-1, linear interpolate")
rm(fdi)

#------------------------------------------------------------------------------------------------#
# tourism; number of arrivals from World Bank Group; https://api.worldbank.org/v2/en/indicator/ST.INT.ARVL?downloadformat=excel
#------------------------------------------------------------------------------------------------#  

# bring in datasets; Number of Arrivals 
url <- "https://api.worldbank.org/v2/en/indicator/ST.INT.ARVL?downloadformat=excel"
destfile <- "ST_INT.xls"
curl::curl_download(url, destfile)
arrivals_per_year <- read_excel(destfile, skip = 2)
rm(url, destfile)

# clean data
arrivals_per_year <- arrivals_per_year %>%
  rename("country" = "Country Name") %>% #rename variable names
  select(-"Country Code", -"Indicator Name", -"Indicator Code") %>% #select relevant data
  pivot_longer(
    cols = -c(country),
    names_to = "year",
    values_to = "arrivals"
  ) %>% #pivot so years and arrivals are in columns
  mutate(year = as.integer(year)) %>% #convert year to integer 
  mutate(year=year+1) %>%
  mutate(arrivals = as.integer(arrivals)) #convert arrivals to integer

# merge in ccodes 
arrivals_per_year <- arrivals_per_year %>%
  mutate(country=ifelse(country=="Turkiye", "Turkey", country)) %>%
  mutate(country=ifelse(country=="Somalia, Fed. Rep.", "Somalia", country)) %>%
  left_join(ccodes, by = c("country", "year")) 

check <- arrivals_per_year %>%
  filter(is.na(ccode)) %>%
  select(-year, -arrivals) %>%
  distinct() #looks good after fixing Turkey above
rm(check)
arrivals_per_year <- arrivals_per_year %>%
  drop_na(ccode) %>% # drop rows that do not have a set country code 
  select("ccode", "year", "arrivals") %>% # remove country names
  mutate(month=12)

# merge into base_data
base_data <- base_data %>%
  left_join(arrivals_per_year, by = c("ccode", "year", "month"))
rm(arrivals_per_year)

#use linear interpolation, 01/1990-04/2026
base_data <- base_data %>%
  mutate(arrivals_OG=arrivals) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    time_id = year + (month - 1) / 12,
    arrivals = if_else(
      year >= 1990 & (year < 2026 | (year == 2026 & month <= 4)),
      na.approx(arrivals, x = time_id, na.rm = FALSE, rule = 2),
      arrivals
    )
  ) %>%
  mutate(arrivals=log10(arrivals+1)) %>%
  ungroup() %>%
  set_variable_labels(arrivals="tourism, log10, t-1, linear inter") %>%
  select(-time_id)

#------------------------------------------------------------------------------------------------#
#add cold war dummy
#------------------------------------------------------------------------------------------------#  

base_data <- base_data %>%
  mutate(cold=ifelse(year<=1989, 1, 0))

#------------------------------------------------------------------------------------------------#
#add pres visits; data collected by Kade in Mar/Apr 2025
#------------------------------------------------------------------------------------------------#  
pres <- read_delim("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/data/presidential_visits.txt", 
                   delim = "\t", escape_double = FALSE, 
                   col_names = FALSE, trim_ws = TRUE)
#rename columns
pres <- pres %>% 
  dplyr::rename(country = X1, city = X2, visit = X3, date = X4)
#select relevant variables
pres <- pres %>%
  dplyr::select(-city) %>%
  dplyr::mutate(visit = 1)  #change reason into one (binary for visit or no visit)
#parsing dates 
pres <- pres %>% 
  mutate(
    month = str_extract(date, "^[A-Za-z]+"),  # Extract month
    year = str_extract(date, "\\d{4}")        # Extract year
  ) %>% 
  dplyr::select(-date) 
pres <- pres %>%
  dplyr::mutate(month = match(month, month.name)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(month=as.numeric(month))
pres <- pres %>%
  left_join(ccodes, by=c("country", "year"))
pres <- pres %>%
  mutate(ccode=ifelse(country=="China, People’s Republic of", 710, ccode)) %>%
  mutate(ccode=ifelse(country=="Republic of China", 710, ccode)) %>%
  mutate(ccode=ifelse(country=="United Kingdom (Northern Ireland)", 200, ccode)) %>%
  mutate(ccode=ifelse(country=="United Kingdom (Wales)", 200, ccode)) %>%
  mutate(ccode=ifelse(country=="Germany, Federal Republic of", 260, ccode)) %>%
  mutate(ccode=ifelse(country=="Yugoslavia (Kosovo)", 347, ccode)) %>%
  mutate(ccode=ifelse(country=="Serbia-Montenegro (Kosovo)", 347, ccode)) %>%
  mutate(ccode=ifelse(country=="Macedonia, Former Yugoslav Republic of", 343, ccode)) %>%
  filter(!is.na(ccode)) %>%
  select(-country)
pres <- pres %>%
  distinct()
#merging with base data
base_data <-  base_data %>%
  left_join(pres, by = c("ccode", "month", "year")) 
rm(pres)
#make it visits within the last 12 months...
base_data <- base_data %>%
  group_by(ccode) %>%
  arrange(ccode, year, month) %>%
  mutate(visit=ifelse(is.na(visit), 0, visit)) %>%
  mutate(v0=visit) %>%
  mutate(v1=ifelse(ccode==lag(ccode), lag(visit), visit)) %>%
  mutate(v2=ifelse(ccode==lag(ccode), lag(v1), visit)) %>%
  mutate(v3=ifelse(ccode==lag(ccode), lag(v2), visit)) %>%
  mutate(v4=ifelse(ccode==lag(ccode), lag(v3), visit)) %>%
  mutate(v5=ifelse(ccode==lag(ccode), lag(v4), visit)) %>%
  mutate(v6=ifelse(ccode==lag(ccode), lag(v5), visit)) %>%
  mutate(v7=ifelse(ccode==lag(ccode), lag(v6), visit)) %>%
  mutate(v8=ifelse(ccode==lag(ccode), lag(v7), visit)) %>%
  mutate(v9=ifelse(ccode==lag(ccode), lag(v8), visit)) %>%
  mutate(v10=ifelse(ccode==lag(ccode), lag(v9), visit)) %>%
  mutate(v11=ifelse(ccode==lag(ccode), lag(v10), visit)) %>%
  mutate(v0=ifelse(is.na(v0), 0, v0)) %>%
  mutate(v1=ifelse(is.na(v1), 0, v1)) %>%
  mutate(v2=ifelse(is.na(v2), 0, v2)) %>%
  mutate(v3=ifelse(is.na(v3), 0, v3)) %>%
  mutate(v4=ifelse(is.na(v4), 0, v4)) %>%
  mutate(v5=ifelse(is.na(v5), 0, v5)) %>%
  mutate(visit=v0+v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11) %>%
  mutate(visit=ifelse(visit>=1, 1, visit)) %>%
  mutate(visit=ifelse(is.na(visit), 0, visit)) %>%
  ungroup() %>%
  select(-v0, -v1, -v2, -v3, -v4, -v5, -v6, -v7, -v8, -v9, -v10, -v11)

#------------------------------------------------------------------------------------------------#
#add regions
#------------------------------------------------------------------------------------------------#  

#get regions from WDI
df <- WDI(indicator = c("CC.EST", "CC.PER.RNK"), country = "all", start = 1960, end = 2023, extra = TRUE)
df <- df %>%
  select(country, region) %>%
  distinct() 
ccodes2 <- ccodes %>%
  select(-year) %>%
  distinct()
df <- df %>%
  left_join(ccodes2, by=c("country"))
rm(ccodes2)
df <- df %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(country=="Somalia, Fed. Rep.", 520, ccode)) %>%
  filter(!is.na(ccode)) %>%
  select(-country) %>%
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
  select(-wdi_region)
base <- base_data %>%
  left_join(df, by=c("ccode"))
#fix missing
base <- base %>%
  mutate(euro_cent_asia=ifelse(country=="Czech Republic", 1, euro_cent_asia)) %>%
  mutate(euro_cent_asia=ifelse(country=="Czechoslovakia", 1, euro_cent_asia)) %>%
  mutate(euro_cent_asia=ifelse(country=="German Democratic Republic", 1, euro_cent_asia)) %>%
  mutate(euro_cent_asia=ifelse(country=="German Federal Republic", 1, euro_cent_asia)) %>%
  mutate(e_asia_pacific=ifelse(country=="Republic of Vietnam", 1, e_asia_pacific)) %>%
  mutate(e_asia_pacific=ifelse(country=="Taiwan", 1, e_asia_pacific)) %>%
  mutate(e_asia_pacific=ifelse(country=="Vietnam", 1, e_asia_pacific)) %>%
  mutate(Sub_africa=ifelse(country=="Zanzibar", 1, Sub_africa)) %>%
  mutate(MENA=ifelse(country=="Yemen Arab Republic", 1, MENA)) %>%
  mutate(MENA=ifelse(country=="Yemen People's Republic", 1, MENA))
base <- base %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))
base_data <- base
rm(base, df)

#------------------------------------------------------------------------------------------------#
#add trade
#------------------------------------------------------------------------------------------------#  

#Start with COW; monadic
url <- "https://correlatesofwar.org/wp-content/uploads/COW_Trade_4.0.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
cow <- read_csv("data/COW_Trade_4.0/National_COW_4.0.csv")
unlink("data.zip")
unlink("data", recursive=TRUE)
rm(url)

cow <- cow %>%
  mutate(year=year+1) %>% #just lagged
  mutate(trade=(imports+exports)) %>%
  mutate(ltrade=log(trade+1)) %>%
  select(ccode, statename, year, trade, ltrade)

#merge to base_yearly for all to make splicing make sense; then put into base_data (monthly)
142344/12 #yearly DF should have around 11,862 obs
yearly <- base_data %>%
  select(country, ccode, year) %>%
  distinct() %>%
  arrange(ccode, year)
yearly <- yearly %>%
  left_join(cow, by=c("ccode", "year"))
rm(cow)
yearly <- yearly %>%
  select(-statename)
ch <- yearly %>%
  select(ccode, year) %>%
  distinct() #no duplicates, we're good
rm(ch)

#add trade w/ US only from COW
url <- "https://correlatesofwar.org/wp-content/uploads/COW_Trade_4.0.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
cow <- read_csv("data/COW_Trade_4.0/Dyadic_COW_4.0.csv")
unlink("data.zip")
unlink("data", recursive=TRUE)
rm(url)
#clean; note that these are not directed dyads
cow <- cow %>%
  filter(ccode1==2) %>%
  mutate(year=year+1) %>%
  select(ccode=ccode2, flow1, flow2, year) %>%
  mutate(flow1=ifelse(flow1<0, 0, flow1)) %>%
  mutate(flow2=ifelse(flow2<0, 0, flow2)) %>%
  mutate(dtrade=flow1+flow2) %>%
  mutate(ldtrade=log(dtrade+1)) %>%
  select(-flow1, -flow2)
ch <- cow %>%
  select(ccode, year) %>%
  distinct() #no duplicates, we're good
rm(ch)
yearly <- yearly %>%
  left_join(cow, by=c("ccode", "year"))
rm(cow)

#now do WDI, monadic

#Add WDI; monadic
indicators <- c("NE.EXP.GNFS.CD", "NE.IMP.GNFS.CD")
wdi <- WDI(indicator = indicators, start = 1960, end = 2026, extra = TRUE)
rm(indicators)
wdi <- wdi %>%
  select(country, year, NE.EXP.GNFS.CD, NE.IMP.GNFS.CD) %>%
  rename(exports = NE.EXP.GNFS.CD) %>%
  rename(imports = NE.IMP.GNFS.CD) %>%
  mutate(imports=ifelse(is.na(imports), 0, imports)) %>%
  mutate(exports=ifelse(is.na(exports), 0, exports)) %>%
  mutate(year=year+1) %>%
  mutate(wdi_trade=(imports+exports)) %>%
  mutate(wdi_ltrade=log(wdi_trade+1))
ch <- wdi %>%
  select(country, year) %>%
  distinct() #no duplicates, we're good
rm(ch)
wdi <- wdi %>%
  left_join(ccodes, by=c("country", "year"))
wdi <- wdi %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(country=="Somalia, Fed. Rep.", 520, ccode)) %>%
  rename(wdi_country=country) %>%
  filter(wdi_trade>0) %>%
  filter(!is.na(ccode)) %>%
  select(-exports, -imports)
ch <- wdi %>%
  select(ccode, year) %>%
  distinct() #no duplicates, we're good
rm(ch)
yearly <- yearly %>%
  left_join(wdi, by=c("ccode", "year"))
yearly <- yearly %>%
  select(-wdi_country)
rm(wdi)
ch <- yearly %>%
  select(ccode, year) %>%
  distinct() #looks good
rm(ch)

#add trade w/ US only; from https://dataweb.usitc.gov/; couldn't pull these directly from web so putting them on github
#grabbed updated data on 03/27/26
url <- "https://github.com/thynec/CoupCats/raw/refs/heads/data/updated-DataWeb-Query-Export%20(2).xlsx"
destfile <- "DataWeb_Query_Export_20_1_.xlsx"
curl::curl_download(url, destfile)
exports <- read_excel(destfile, skip = 2, sheet = "FAS Value")
rm(destfile, url)
exports <- exports %>%
  select(-"Data Type") %>%
  pivot_longer(cols=-Country,
               names_to="year",
               values_to="exports") %>%
  rename(country=Country) %>% 
  mutate(year=as.numeric(year)) %>%
  mutate(exports=as.numeric(exports)) %>%
  mutate(year=year+1) 
exports <- exports %>%
  left_join(ccodes, by=c("country", "year")) %>%
  arrange(ccode, year, -exports) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) %>%
  filter(problem!=1) %>%
  select(-problem, -country) 

url <- "https://github.com/thynec/CoupCats/raw/refs/heads/data/updated_imports_DataWeb-Query-Export%20(2).xlsx"
destfile <- "DataWeb_Query_Export_20_1_.xlsx"
curl::curl_download(url, destfile)
imports <- read_excel(destfile, skip = 2, sheet = "General Customs Value")
rm(destfile, url)
imports <- imports %>%
  select(-"Data Type") %>%
  pivot_longer(cols=-Country,
               names_to="year",
               values_to="imports") %>%
  rename(country=Country) %>% 
  mutate(year=as.numeric(year)) %>%
  mutate(imports=as.numeric(imports)) %>%
  mutate(year=year+1) 
imports <- imports %>%
  left_join(ccodes, by=c("country", "year")) %>%
  arrange(ccode, year, -imports) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) %>%
  filter(problem!=1) %>%
  select(-problem)

usitc <- exports %>% 
  full_join(imports, by=c("ccode", "year"))
usitc <- usitc %>%
  mutate(exports=ifelse(is.na(exports), 0, exports)) %>%
  mutate(imports=ifelse(is.na(imports), 0, imports)) %>%
  filter(year<=2026) %>%  mutate(usitc_dtrade=imports+exports) %>%
  mutate(usitc_ldtrade=log(imports+exports+1)) %>%
  select(-imports, -exports) 
usitc <- usitc %>%
  mutate(ccode=ifelse(country=="Côte d`Ivoire", 437, ccode)) %>%
  mutate(ccode=ifelse(country=="São Tomé and Príncipe", 403, ccode)) %>%
  mutate(ccode=ifelse(country=="Czechia (Czech Republic)", 316, ccode)) %>%
  mutate(ccode=ifelse(country=="Eswatini (Swaziland)", 572, ccode))
usitc <- usitc %>%
  filter(!is.na(ccode))
usitc <- usitc %>%
  select(-country)

yearly <- yearly %>%
  left_join(usitc, by=c("ccode", "year"))
rm(usitc, exports, imports)

cor(yearly$ldtrade, yearly$usitc_ldtrade, use="complete.obs")
cor(yearly$dtrade, yearly$usitc_dtrade, use="complete.obs")
#above very high correlations so getting at the same thing; okay to splice

#splice monadic
mon <- yearly %>%
  group_by(ccode) %>%
  select(country, ccode, year, ltrade, wdi_ltrade) %>%
  mutate(ch=(wdi_ltrade-lag(wdi_ltrade))/lag(wdi_ltrade)) %>%
  mutate(splice=ltrade) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice))
mon <- mon %>%
  select(ccode, year, ltrade=splice)

#splice dyadic
dy <- yearly %>%
  group_by(ccode) %>%
  select(country, ccode, year, ldtrade, usitc_ldtrade) %>%
  filter(ccode!=2) %>%
  mutate(splice=ldtrade) %>%
  mutate(ch=(usitc_ldtrade-lag(usitc_ldtrade))/lag(usitc_ldtrade)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice)) %>%
  select(ccode, year, ldtrade=splice)
yearly <- yearly %>%
  select(ccode, year) %>%
  left_join(mon, by=c("ccode", "year")) %>%
  left_join(dy, by=c("ccode", "year")) %>% 
  mutate(month=12)
base_data <- base_data %>%
  left_join(yearly, by=c("ccode", "year", "month"))
rm(dy, mon, yearly)

#interpolate
base_data <- base_data %>%
  mutate(ltrade_OG=ltrade) %>%
  set_variable_labels(ltrade_OG = "total trade, log10, t-1") %>%
  mutate(ldtrade_OG=ldtrade) %>%
  set_variable_labels(ldtrade_OG = "trade w/ US, log10, t-1")
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    time_id = year + (month - 1)/12,
    ltrade = na.approx(
      ltrade,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  mutate(
    ldtrade = na.approx(
      ldtrade,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  ungroup() %>%
  select(-time_id)


#-----------------------------------------------------------------# 
#KOF Globalization 
#-----------------------------------------------------------------#

library(readr)


kof_global <- read_csv("https://github.com/catalinahix06-star/CoupCats/raw/refs/heads/main/KOFGI_2025_public(Sheet1).csv")


kof_global <- kof_global %>%
  select(KOFTrGIdf, #trade globalization, de facto
         KOFPoGIdj, #political globalization, de jure
         KOFCuGIdf, #gender parity 
         KOFIpGIdf, #interpersonal globalization de facto
         country, 
         year) %>%
  mutate(month = list(1:12)) %>%  #expand to monthly  
  unnest(month) %>%
  mutate(across(
    c(KOFTrGIdf, 
      KOFPoGIdj, 
      KOFCuGIdf, 
      KOFIpGIdf), 
      ~ ifelse(month == 12, .x, NA)
  )) %>% 
  mutate(year=year+1) %>% #lagging
  mutate(across(c
                (KOFTrGIdf, 
                  KOFPoGIdj, 
                  KOFCuGIdf, 
                  KOFIpGIdf),
                ~ .x / 100))  #need to make the percentages back to regular numbers 
view(kof_global) 

#Merging into base data
base_data <- base_data %>%
  left_join(
    kof_global %>%
      select(
        country,
        year,
        month,
        KOFTrGIdf,
        KOFPoGIdj,
        KOFCuGIdf,
        KOFIpGIdf
      ),
    by = c("country", "year", "month")
  ) 
view(base_data) 

#------------------------------------------------------------------------------------------------#
# Add regional contagion. 
#------------------------------------------------------------------------------------------------#  

# Bring in COW direct contiguity 
url <- "https://correlatesofwar.org/wp-content/uploads/DirectContiguity320.zip"
download.file(url, "DirectContiguity320.zip")
unzip("DirectContiguity320.zip", exdir="DirectContiguity320")
unlink("DirectContiguity320.zip")
border <- read_csv("DirectContiguity320/DirectContiguity320/contdir.csv")
unlink("data", recursive=TRUE)
rm(url)  

#Clean COW contiguity
border <- border %>%
  select(-dyad, -statelab, -statehab, -notes, -version) %>%
  rename(ccode = statelno, #primary state
         neighbor = statehno, #state bordering primary state
         border_type = conttype)
#raw data in form of dyads; creating two way relationships
border_reversed <- border %>%
  select(ccode, neighbor, border_type, begin, end) %>%
  rename(
    ccode_old = ccode,
    neighbor_old = neighbor
  ) %>%
  mutate(
    ccode = neighbor_old,
    neighbor = ccode_old
  ) %>%
  select(ccode, neighbor, border_type, begin, end)
border <- bind_rows(border, border_reversed) %>%
  distinct() 
rm(border_reversed)

#expand to monthly
border <- border %>%
  mutate(
    start_id = (begin %/% 100) * 12 + (begin %% 100) - 1,
    end_id  = (end   %/% 100) * 12 + (end   %% 100) - 1,
    n_months = end_id - start_id + 1
  )
border_expanded <- border %>%
  uncount(n_months, .id = "month_offset") %>%
  mutate(
    month_id = start_id + month_offset - 1,
    year = month_id %/% 12,
    month = month_id %% 12 + 1
  ) %>%
  select(ccode, neighbor, border_type, year, month) %>%
  filter(year >= 1950) 
rm(border)

# Bringing in coup data.
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) 
coup_data <- coup_data %>%
  mutate(coup=ifelse(ccode==451 & year==1967 & month==3, 2, coup))
coup_data <- coup_data %>% 
  select(-ccode_gw, -ccode_polity, -day, -version) %>%
  distinct() %>% #dropped the obs where 2 coups in same month, as above
  mutate(coup_attempt = 1) %>%
  mutate(coup_successful=ifelse(coup==2, 1, 0)) %>%
  mutate(coup_failed=ifelse(coup==1, 1, 0))

#Data about border relations until 2016. Assuming border relations stay the same post last update, changing end date to current end of data, March 2026
dyads_to_extend <- border_expanded %>%
  group_by(ccode, neighbor, border_type) %>%
  summarise(
    last_date = max(year * 100 + month),
    .groups = "drop"
  ) %>%
  filter(last_date == 201612)
extensions <- dyads_to_extend %>%
  crossing(
    year  = rep(2017:2026, each = 12),
    month = rep(1:12, times = 9)
  ) %>%
  filter(!(year == 2026 & month >= 5))
border_expanded <- bind_rows(border_expanded, extensions) %>%
  arrange(ccode, neighbor, border_type, year, month) %>%
  select(-last_date)
rm(dyads_to_extend, extensions)

#merging in coup data
regional_contagion <- border_expanded %>% 
  left_join(coup_data, by = c("year","month", "neighbor" = "ccode")) %>%
  select(-country, -coup) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))
rm(border_expanded, coup_data)

#creating contagion variables
regional_contagion <- regional_contagion %>%
  mutate(neighboring_coup_attempt = case_when(coup_attempt == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(neighboring_coup = case_when(coup_successful == 1 ~ 1, TRUE ~ 0)) %>%
  select(-border_type, -coup_successful, -coup_failed, -coup_attempt)
regional_contagion <- regional_contagion %>%
  group_by(ccode, year, month) %>%
  summarise(
    neighboring_coup = as.numeric(any(neighboring_coup == 1)),
    neighboring_coup_attempt = as.numeric(any(neighboring_coup_attempt == 1)),
    .groups = "drop"
  )

#Creating last 5 years congation
regional_contagion <- regional_contagion %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    cum_coup = cumsum(neighboring_coup),
    coup_last_5yrs = ifelse(
      lag(cum_coup, 1, default = 0) - lag(cum_coup, 60, default = 0) > 0,
      1, 0
    )
  ) %>%
  select(-cum_coup) %>%
  ungroup()
regional_contagion <- regional_contagion %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    cum_coup = cumsum(neighboring_coup_attempt),
    coup_attempt_last_5yrs = ifelse(
      lag(cum_coup, 1, default = 0) - lag(cum_coup, 60, default = 0) > 0,
      1, 0
    )
  ) %>%
  select(-cum_coup) %>%
  ungroup()

# Merging into base data. 
base_data <- base_data %>%
  left_join(regional_contagion, by = c("year", "month", "ccode"))
rm(regional_contagion)

#------------------------------------------------------------------------------------------------#
# International Governmental Organizations(IGO)
#------------------------------------------------------------------------------------------------#  

#getting the data from COW
url <-  "https://correlatesofwar.org/wp-content/uploads/state_year_formatv3.zip"
download.file (url, "igo.zip")#Downloads the dataset and saves it as data.zip in working directory.
unzip ("igo.zip", exdir = "igo") #Extracts the contents of the ZIP file into a folder named data.
igo <- read_csv("igo/state_year_formatv3.csv")
rm(url)
unlink("igo.zip", recursive = TRUE )
unlink("igo", recursive = TRUE )

igo_OG <- igo

# creating dummy variable for states who are full members of any int organization that year
igo <- igo %>%
  mutate(member = if_any(AAAID:Wassen, ~ . == 1)) %>%  #checks if any column from AAAID to Wassen is equal to 1 for that row, if yes, it calls it TRUE
  relocate(member, .after = state)
igo <- igo %>%
  mutate(member = as.integer(if_any(AAAID:Wassen, ~ . == 1))) #converts TRUE/FALSE to 1/0.

# creating dummy variable for states who are associate members of any int organization that year
igo <- igo %>%
  mutate(associate = if_any(AAAID:Wassen, ~ . == 2)) %>% 
  relocate(associate, .after = member) %>% 
  mutate(associate = as.integer(if_any(AAAID:Wassen, ~ . == 1)))

# creating dummy variable for states who are observers of any int organization that year
igo <- igo %>%
  mutate(observer = if_any(AAAID:Wassen, ~ . == 3)) %>% 
  relocate(observer, .after = associate) %>% 
  mutate(observer = as.integer(if_any(AAAID:Wassen, ~ . == 1)))

igo <- igo %>%
  dplyr::select(ccode, year, member, associate, observer) %>% #keeping only what we care about
  filter(year>1949)
rm(igo, io_counts)

# merging to base data; going to omit this one because almost all=1, so not enough variation to matter.  Instead use below...

#create counts of each IO type
io_counts <- igo_OG %>%
  mutate(
    full = rowSums(across(4:537, ~ . == 1), na.rm = TRUE),
    associate = rowSums(across(4:537, ~ . == 2), na.rm = TRUE),
    observer = rowSums(across(4:537, ~ . == 3), na.rm = TRUE)
  ) %>%
  select(ccode, year, state, full, associate, observer) %>%
  filter(year>=1945) %>%
  mutate(year=year+1) %>%
  mutate(month=12) %>%
  select(-state)

#create a total measure, using full=1, assoc=0.5, observer=0.25
io_counts <- io_counts %>%
  mutate(IOs_sum=full+associate*.5+observer*.25) %>%
  set_variable_labels(IOs_sum="IO count, full=1, assoc=.5, observer=.25")

base_data <- base_data %>%
  left_join(io_counts, by=c("ccode", "year", "month")) %>%
  mutate(full_OG=full) %>%
  mutate(associate_OG=associate) %>%
  mutate(observer_OG=observer) %>%
  mutate(IOs_sum_OG=IOs_sum) 

base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    time_id = year + (month - 1)/12,
    full = na.approx(
      full,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  mutate(
    IOs_sum = na.approx(
      IOs_sum,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  ungroup() %>%
  select(-time_id) %>%
  set_variable_labels(full="# of IO full memberships, t-1, linear inter") %>%
  set_variable_labels(full="IO memberships, full=1, assoc=.5, obs=.25, t-1, linear inter")
rm(igo_OG, io_counts)

#------------------------------------------------------------------------------------------------#
#alliances
#------------------------------------------------------------------------------------------------#  

url <- "http://www.atopdata.org/uploads/6/9/1/3/69134503/atop_5.1__.csv_.zip"
download.file (url, "atop.zip")#Downloads the dataset and saves it as data.zip in working directory.
unzip ("atop.zip", exdir = "atop") #Extracts the contents of the ZIP file into a folder named data.
atop <- read_csv("atop/ATOP 5.1 (.csv)/atop5_1m.csv")
rm(url)
unlink("atop.zip", recursive = TRUE )

atop <- atop %>%
  dplyr::select(member, yrent, moent, dayent, yrexit, moexit, dayexit, defense) %>%
  filter(yrexit >= 1950) %>% #removing alliances that ended before 1950
  rename(ccode = member) %>% #making life easier
  rowwise() %>%
  mutate(
    years = list(seq(yrent, ifelse(is.na(yrexit) | yrexit == 0, 2026, yrexit))),
    months = list(1:12)
  ) %>%
  unnest(years) %>%
  unnest(months) %>%
  filter(
    (years > yrent | (years == yrent & months >= moent)) &
      (is.na(yrexit) | yrexit == 0 | years < yrexit | (years == yrexit & months < moexit) |
         (years == yrexit & months == moexit & (is.na(dayexit) | dayexit == 0)) |
         (yrexit == 0 & moexit == 0 & dayexit == 0))
  ) %>%
  rename(year = years, month = months) %>%
  mutate(
    alliance_active = 1,
    defense_alliance_active = ifelse(defense == 1, 1, 0)
  )

atop <- atop %>% 
  dplyr::select(ccode, year, month, alliance_active, defense_alliance_active) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode, year, month) %>%
  summarise(
    alliance_active = sum(alliance_active, na.rm = TRUE),
    defense_alliance_active = sum(defense_alliance_active, na.rm = TRUE),
    .groups = "drop"
  )

# Merge atop with base_data
base_data <- base_data %>%
  left_join(atop, by = c("ccode", "year", "month")) %>%
  mutate(
    alliance_active = replace_na(alliance_active, 0),
    defense_alliance_active = replace_na(defense_alliance_active, 0)
  )
rm(atop)
#### Important note about alliances data: The ATOP dataset treats alliances that were still valid as of December 31, 2018 (they use yrexit==0), as ongoing. This means that if an alliance ended after 2018, it is still considered ongoing and will be marked as active in the dataset. 

#------------------------------------------------------------------------------------------------#
#                                       US Foreign Assistance
#------------------------------------------------------------------------------------------------# 

# July 1945 - October 2025

aid <- read_csv("https://s3.amazonaws.com/files.explorer.devtechlab.com/us_foreign_aid_country.csv")

us_aid <- aid %>%
  filter(`Transaction Type Name` == "Obligations") %>%
  group_by(`Country Name`, `Fiscal Year`) %>%
  summarise(us_aid = sum(constant_amount, na.rm = TRUE), .groups = "drop") %>%
  rename(
    country = `Country Name`,
    year = `Fiscal Year`
  ) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(year)) %>% 
  left_join(ccodes, by = c("country", "year")) %>%
  mutate(
    date = case_when(
      year < 1977 ~ make_date(year - 1, 7, 1),   # old system prior to 1976: July 1 of prior year
      TRUE        ~ make_date(year - 1, 10, 1)    # new system: Oct 1 of prior year
    ),
    cal_year = year(date),
    month = month(date),
    day = day(date),
    us_aid_log = log1p(pmax(us_aid, 0)),
  ) %>% 
  mutate(ccode = case_when(
    country == "Korea, Democratic Republic of" ~ 731,   
    country == "Czechoslovakia (former)"        ~ 315,   
    TRUE ~ ccode
  )) %>%
  filter(!is.na(ccode)) %>% 
  select(c(ccode, year = cal_year, month, us_aid, us_aid_log))
  

base_data <- base_data %>% 
  left_join(us_aid, by = c("ccode", "year", "month"))
  
rm(aid, us_aid)

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.e.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  
