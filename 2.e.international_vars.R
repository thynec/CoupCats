#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building international-related variables

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
check <- pres %>%
  filter(is.na(ccode)) 
table(check$country) #need to fix several...
rm(check)
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
check <- base %>%
  filter(is.na(MENA))
table(check$country) #got problems; WDI not recognizing old names; easy to fix manually
rm(check)
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
check <- base %>%
  filter(is.na(e_asia_pacific)) %>%
  filter(is.na(MENA)) %>%
  filter(is.na(euro_cent_asia)) %>%
  filter(is.na(LA_carrib)) %>%
  filter(is.na(N_america)) %>%
  filter(is.na(S_asia)) %>%
  filter(is.na(Sub_africa))
table(check$country) #looks good
rm(check)
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
check <- cow %>%
  select(ccode, year) %>%
  distinct() #no duplicates
rm(check)
  
cow <- cow %>%
  mutate(year=year+1) %>% #just lagged
  mutate(imports=ifelse(is.na(imports), 0, imports)) %>%
  mutate(exports=ifelse(is.na(exports), 0, exports)) %>%
  mutate(trade=(imports+exports)) %>%
  mutate(ltrade=log(trade+1)) %>%
  select(ccode, statename, year, trade, ltrade)

#merge to base_yearly for all to make splicing make sense; then put into base_data (monthly)
142344/12 #yearly DF should have around 11,862 obs
yearly <- base_data %>%
  select(country, ccode, year) %>%
  distinct() %>%
  arrange(ccode, year)
check <- yearly %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0))
summary(check) #good; no duplicates
rm(check)
yearly <- yearly %>%
  left_join(cow, by=c("ccode", "year"))
check <- yearly %>%
  filter(country!=statename)
table(check$country) #looks good
rm(check, cow)
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
  wdi <- WDI(indicator = indicators, start = 1960, end = 2025, extra = TRUE)
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
check <- wdi %>%
  filter(is.na(ccode))
table(check$country)
rm(check)
wdi <- wdi %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
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
check <- yearly %>%
  filter(is.na(ccode)) #good
check <- yearly %>%
  filter(country!=wdi_country) %>%
  relocate(wdi_country) %>%
  arrange(ccode, year) %>%
  mutate(drop=ifelse(ccode==lag(ccode), 1, 0)) %>%
  filter(is.na(drop)) #looks fine
yearly <- yearly %>%
  select(-wdi_country)
rm(check, wdi)
ch <- yearly %>%
  select(ccode, year) %>%
  distinct() #looks good
rm(ch)

#add trade w/ US only; from https://dataweb.usitc.gov/; couldn't pull these directly from web so putting them on github
url <- "https://github.com/thynec/CoupCats/raw/refs/heads/data/DataWeb-Query-Export%20(1).xlsx"
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
  mutate(year=year+1) %>%
  filter(!is.na(exports))
check <- exports %>%
  select(country, year) %>%
  distinct() #good
rm(check)
exports <- exports %>%
  left_join(ccodes, by=c("country", "year")) %>%
  arrange(ccode, year, -exports) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) %>%
  filter(problem!=1) %>%
  select(-problem, -country)

url <- "https://github.com/thynec/CoupCats/raw/refs/heads/data/DataWeb-Query-Export%20(2).xlsx"
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
  mutate(year=year+1) %>%
  filter(!is.na(imports))
check <- imports %>%
  select(country, year) %>%
  distinct() #good
rm(check)
imports <- imports %>%
  left_join(ccodes, by=c("country", "year")) %>%
  arrange(ccode, year, -imports) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) %>%
  filter(problem!=1) %>%
  select(-problem)

usitc <- exports %>% 
  full_join(imports, by=c("ccode", "year"))
check <- usitc %>%
  select(ccode, year) %>%
  distinct() #good
rm(check)
check <- usitc %>%
  arrange(ccode, year) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) #looks good
rm(check)
usitc <- usitc %>%
  mutate(exports=ifelse(is.na(exports), 0, exports)) %>%
  mutate(imports=ifelse(is.na(imports), 0, imports)) %>%
  filter(year<=2025) %>%
  mutate(usitc_dtrade=imports+exports) %>%
  mutate(usitc_ldtrade=log(imports+exports+1)) %>%
  select(-imports, -exports) 
usitc <- usitc %>%
  mutate(ccode=ifelse(country=="Côte d`Ivoire", 437, ccode)) %>%
  mutate(ccode=ifelse(country=="São Tomé and Príncipe", 403, ccode)) %>%
  mutate(ccode=ifelse(country=="Czechia (Czech Republic)", 316, ccode)) %>%
  mutate(ccode=ifelse(country=="Eswatini (Swaziland)", 572, ccode))
check <- usitc %>%
  filter(is.na(ccode))
table(check$country) #looks good
rm(check)
usitc <- usitc %>%
  filter(!is.na(ccode))
check <- usitc %>%
  arrange(ccode, year) %>%
  mutate(prob=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) #looks good
rm(check)
check <- usitc %>%
  select(ccode, year) %>%
  distinct() #looks good
rm(check)
usitc <- usitc %>%
  select(-country)

yearly <- yearly %>%
  left_join(usitc, by=c("ccode", "year"))
rm(usitc, exports, imports)

check <- yearly %>%
  select(ccode, year) %>%
  distinct() #looks good
check <- yearly %>%
  mutate(prob=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) #looks good
rm(check)

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
  mutate(splice=ifelse(is.na(splice) & ccode==lag(ccode), lag(splice)*ch+lag(splice), splice))
mon <- mon %>%
  group_by(ccode) %>%
  filter(any(!is.na(splice))) %>%
  mutate(splice=na.approx(splice, year, rule=2, na.rm=TRUE)) %>%
  ungroup() %>%
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
  select(ccode, year, ldtrade=splice)
yearly <- yearly %>%
  select(ccode, year) %>%
  left_join(mon, by=c("ccode", "year")) %>%
  left_join(dy, by=c("ccode", "year"))
base_data <- base_data %>%
  left_join(yearly, by=c("ccode", "year"))
check <- base_data %>%
  select(ccode, year, month) %>%
  distinct() #looks good
rm(check, dy, mon, yearly)

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.e.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  


#------------------------------------------------------------------------------------------------#
# Add regional contagion. 
#------------------------------------------------------------------------------------------------#  

# Bringing in border states. 
border_states <- create_dyadyears() %>%
  add_contiguity() %>%
  filter(year >= 1950) %>%
  filter(conttype %in% c(1, 2)) # Land borders and very close maritime borders. 
border_states <- border_states %>%
  rename(ccode = ccode1,
         neighbor = ccode2,
         border_type = conttype) # Where ccode is primary state, neighbor is coup-victim. 


# Bringing in coup data.
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) 
coup_data <- coup_data %>% 
  mutate(coup = ifelse(ccode == 451 & year == 1967 & month == 3, 2, coup),
         coup_successful = ifelse(coup == 2, 1, NA),
         coup_failed = ifelse(coup == 1, 1, NA),
         coup_attempt = 1) %>%
  select(-ccode_gw, -ccode_polity, -day, -version) %>%
  distinct() %>% 
  mutate(coup_attempt = ifelse(is.na(coup_attempt) & year >= 1950, 0, as.numeric(coup_attempt)),
         coup_successful = ifelse(is.na(coup_successful) & year >= 1950, 0, as.numeric(coup_successful)),
         coup_failed = ifelse(is.na(coup_failed) & year >= 1950, 0, as.numeric(coup_failed)))

# Merging data sets. 
regional_contagion <- border_states %>% 
  left_join(coup_data, by = c("year", "neighbor" = "ccode"), relationship = "many-to-many") %>%
  select(-country) %>%
  mutate(neighbor = labelled(neighbor, label = "coup states")) %>%
  drop_na()
rm(coup_data, border_states)

# Mutating regional contagion. 
regional_contagion <- regional_contagion %>%
  mutate(neighboring_coup = case_when(coup_attempt == 1 ~ 1, TRUE ~ 0), relationship = "many-to-many") %>%
  select(ccode, month, year, neighbor, neighboring_coup)

# Merging into base data. 
base_data <- base_data %>%
  left_join(regional_contagion, by = c("year", "month", "ccode"), relationship = "many-to-many") %>%
  mutate(neighbor = replace_na(neighbor, 0)) %>%
  mutate(neighboring_coup = replace_na(neighboring_coup, 0))

#------------------------------------------------------------------------------------------------#
#add tourism
#------------------------------------------------------------------------------------------------#  

#Getting data - Tourism (From World Banks)
url <- "https://api.worldbank.org/v2/en/indicator/ST.INT.ARVL?downloadformat=excel"
destfile <- "ST_INT.xls"
curl::curl_download(url, destfile)
tourism <- read_excel(destfile, skip = 3)
# I couldn't find data from before 1995, included this here just in case

#Reshaping data
tourism <- tourism %>%
  rename( "country" = `Country Name`) %>% 
  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`))  #removing things we don't want
tourism <- tourism %>%
  pivot_longer(
    cols = -c(country),  # Keep country-related columns fixed
    names_to = "year",  
    values_to = "int_arrivals"
  ) %>%
  mutate(year = as.integer(year))  # Convert Year to integer

#Merging to base
tourism <- tourism %>%
  left_join(ccodes, by=c("country", "year"))
tourism <- tourism %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode))
base_data <- base_data %>%
  left_join(tourism, by=c("ccode", "year"))

  
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
    years = list(seq(yrent, ifelse(is.na(yrexit) | yrexit == 0, 2025, yrexit))),
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
  dplyr::select(ccode, year, month, alliance_active, defense_alliance_active)

# Merge atop with base_data
base_data <- base_data %>%
  left_join(atop, by = c("ccode", "year", "month")) %>%
  mutate(
    alliance_active = replace_na(alliance_active, 0),
    defense_alliance_active = replace_na(defense_alliance_active, 0)
  )
rm(atop)

#### Important note about alliances data: The ATOP dataset treats alliances that were still valid as of December 31, 2018 (they use yrexit==0), as ongoing. This means that if an alliance ended after 2018, it is still considered ongoing and will be marked as active in the dataset. 



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

copdab <-  copdab %>% 
  dplyr::select(year,target, month, z_variable)

# 3. Merging both
int_signals <- weis %>%
  full_join(copdab, by=c("target", "year", "month", "z_variable"))

int_signals <-int_signals %>% 
  group_by(year, target, month) %>% 
  mutate(z= mean(z_variable))

int_signals <-int_signals %>% 
  select(-z_variable)

int_signals <- distinct(int_signals) 

base_data <- base_data %>% 
  left_join(int_signals, by = c("ccode" = "target", "year" = "year", "month" = "month"))

base_data <- base_data %>% 
  mutate(z = replace_na(z, 0)) %>%  #turning NAs to zeros
  rename(int_signal = z) 


###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.e.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  

# ------------------- FDI (World Bank) ------------------- #
# Bring in FDI (World Bank). 
fdi_data <- WDI(indicator = "BX.KLT.DINV.CD.WD", start = 1960, end = 2024, extra = TRUE)

# Cleaning up FDI. 
fdi_data <- fdi_data %>% 
  select(country, year, BX.KLT.DINV.CD.WD) %>%
  rename(fdi = BX.KLT.DINV.CD.WD) %>%
  left_join(ccodes, by =  c("country", "year")) %>%
  select(-country)

# Merging into base data. 
base_data <- base_data %>%
  left_join(fdi_data, by = c("ccode", "year"))
rm(fdi_data)

