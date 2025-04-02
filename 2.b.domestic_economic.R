#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building domestic economic variables

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


# Country codes (Thyne 2022). 
url <- "https://www.uky.edu/~clthyn2/replace_ccode_country.xls" # Bringing in ccodes to merge. 
destfile <- "replace_ccode_country.xls"
curl::curl_download(url, destfile)
ccodes <- read_excel(destfile)
rm(url, destfile)

#------------------------------------------------------------------------------------------------#  
#bring in all vdem relevant data; clean it up
#------------------------------------------------------------------------------------------------#  

#'The V-Dem dataset does not cover some countries, namely: Andorra, Antigua and Barbuda, Bahamas, Belize, Brunei, Dominica, Federated States of Micronesia, Grenada, Kiribati, Liechtenstein, Marshall Islands, Monaco, Nauru, Palau, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines, Samoa, San Marino, Tonga, Tuvalu, and the Vatican.'
vdem_og <- vdem
vdem <- vdem_og
rm(vdem_og)
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
  mutate(year=year+1) %>% #just lagged
  filter(year >= 1950)
vdem <- vdem %>% # Merging in ccodes. 
  left_join(ccodes, by = c("year", "country")) %>%
  filter(!is.na(ccode)) # Non-state actors. 

#------------------------------------------------------------------------------------------------#      
#building gdp/cap measure
#------------------------------------------------------------------------------------------------#      
#start with Vdem; already lagged from above
vdem_gdppc <- vdem %>%
  subset(select = c(country, ccode, year, gdppc)) %>%
  rename(vdem_gdppc = gdppc,
         c_merge = country) 
base_data <- base_data %>%
  left_join(vdem_gdppc, by = c("ccode", "year"))
check <- base_data %>% 
  subset(select = c(country, c_merge, year)) %>%
  distinct() %>%
  filter(country!=c_merge)
rm(check) # All good. 
base_data <- base_data %>%
  subset(select = -c(c_merge))
rm(vdem_gdppc, vdem)
#Now deal with WDI
wdi_gdppc <- WDI(country = "all",
                 indicator = "NY.GDP.PCAP.CD", 
                 start = 1960, 
                 end = 2024,
                 extra = TRUE)
wdi_gdppc <- wdi_gdppc %>%
  subset(select = c(country, year, NY.GDP.PCAP.CD)) %>%
  rename(wdi_gdppc = NY.GDP.PCAP.CD) %>%
  mutate(year=year+1) %>% #just lagged
  left_join(ccodes, by = c("year", "country")) %>%
  rename(c_merge = country)
base_data <- base_data %>%
  left_join(wdi_gdppc, by = c("ccode", "year")) 
check <- base_data %>% 
  subset(select = c(country, c_merge)) %>%
  distinct() %>%
  filter(country!=c_merge)
rm(check) # All good. 
base_data <- base_data %>%
  subset(select = -c(c_merge))
rm(wdi_gdppc)
#Splicing vdem+wdi for full years
df <- base_data %>%
  select(country, ccode, year, vdem_gdppc, wdi_gdppc) %>%
  distinct()
df <- df %>%
  group_by(ccode) %>%
  arrange(ccode, year) %>%
  mutate(change=(wdi_gdppc-lag(wdi_gdppc))) %>%
  mutate(now=vdem_gdppc) %>%
  mutate(perc_change=change/lag(wdi_gdppc)) %>%
  mutate(gdppc=ifelse(is.na(now), lag(now)*perc_change+lag(vdem_gdppc), now)) %>%
  mutate(gdppc=ifelse(is.na(gdppc), lag(gdppc)*perc_change+lag(gdppc), gdppc)) %>%
  mutate(gdppc=ifelse(is.na(gdppc), lag(gdppc)*perc_change+lag(gdppc), gdppc)) %>%
  mutate(gdppc=ifelse(is.na(gdppc), lag(gdppc)*perc_change+lag(gdppc), gdppc)) 
check <- df %>%
  filter(year>2020)
cor(check$gdppc, check$wdi_gdppc, use="complete.obs") #looks good
rm(check)
hist(df$gdppc) #need to log
df <- df %>%
  mutate(lgdppcl=log(gdppc))
hist(df$gdppc) #looks good
df <- df %>%
  select(ccode, year, gdppc, lgdppcl)
#create % change in gdp/cap using non-logged data
df <- df %>%
  group_by(ccode) %>%
  arrange(ccode, year) %>%
  mutate(ch_gdppcl=((gdppc-lag(gdppc))/lag(gdppc))) %>% #huge range but seems legit
  select(-gdppc)
df <- df %>%
  group_by(ccode) %>%
  mutate(lgdppcl = if (sum(!is.na(lgdppcl)) >=2) na.approx(lgdppcl, year, rule=2) else lgdppcl) %>%
  mutate(ch_gdppcl = if (sum(!is.na(ch_gdppcl)) >=2) na.approx(ch_gdppcl, year, rule=2) else ch_gdppcl) %>%
  ungroup()
#merge to base
base_data <- base_data %>%
  select(-vdem_gdppc, -wdi_gdppc) %>%
  left_join(df, by=c("ccode", "year")) %>%
  set_variable_labels(
    lgdppcl="GDP/cap, WDI+vdem, t-1, log",
    ch_gdppcl="%ch GDP/cap, WDI+vdem, t-1")
rm(df)  

#------------------------------------------------------------------------------------------------#      
#building CPI
#------------------------------------------------------------------------------------------------# 

#Bringing in CPI data from world bank, using 2010 as base year
url <- "https://api.worldbank.org/v2/en/indicator/FP.CPI.TOTL?downloadformat=excel"
destfile <- "FP_CPI.xls"
curl::curl_download(url, destfile)
FP_CPI <- read_excel(destfile)
View(FP_CPI)

#Cleaning up dataset
colnames(FP_CPI) <- FP_CPI[3, ]
FP_CPI <- FP_CPI[-c(1:3), ]
FP_CPI <- FP_CPI %>% #rearranging data to correct format
  pivot_longer(cols = `1960`:`2023`,  
               names_to = "Year",     
               values_to = "CPI")   
FP_CPI$Year <- as.numeric(FP_CPI$Year) #changing Year to numeric
FP_CPI <- FP_CPI %>% 
  select(-`Indicator Name`, -`Indicator Code`, -`Country Code`) %>% #deleting unnecessary columns
  mutate(Year=Year+1) %>% #Lag CPI
  rename(year = Year) %>%
  rename(country = `Country Name`) %>% 
  left_join(ccodes, by = c("year", "country")) %>% #merging in ccodes
  filter(!is.na(ccode))
FP_CPI <- FP_CPI %>% 
  select(-`country`)
base_data <- base_data %>%
  left_join(FP_CPI, by = c("ccode", "year"))


#Bringing in Thyne/Mitchell dataset
url <- "http://www.uky.edu/~clthyn2/mitchell_thyne_CMPS2010.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
CPI <- read_dta("data/mitchell_thyne_CMPS2010/mitchell_thyne_cmps1.dta")

CPI <- CPI %>%
  select(ccode1, ccode2, year, CPI_issue, CPI)

#------------------------------------------------------------------------------------------------#      
#building Natural Resource Rents, Natural Gas Rents, Debt Service
#------------------------------------------------------------------------------------------------# 
# Pull variables from World Bank API
ntg_data <- wb_data(
  indicator = "NY.GDP.NGAS.RT.ZS",  # Natural Gas Rents
  country = "all",  # Use "all" for all countries
  start_date = 1950,
  end_date = 2025
)
ntr_data <- wb_data(
  indicator = "NY.GDP.TOTL.RT.ZS",  # Natural Resource Rents
  country = "all",  # Use "all" for all countries
  start_date = 1950,
  end_date = 2025
)
dbt_data <- wb_data(
  indicator = "DT.TDS.DECT.GN.ZS",  # Total Debt % of GNI
  country = "all",  # Use "all" for all countries
  start_date = 1950,
  end_date = 2025
)

# Remove unwanted variables
ntg_data <- ntg_data %>%
  select(-unit, -obs_status, -footnote, -last_updated, -iso2c, -iso3c)
ntr_data <- ntr_data %>%
  select(-unit, -obs_status, -footnote, -last_updated, -iso2c, -iso3c)
main <- dbt_data %>%
  select(-unit, -obs_status, -footnote, -last_updated, -iso2c, -iso3c) %>%
  left_join(ntg_data, by = c("country", "date")) %>%
  left_join(ntr_data, by = c("country", "date")) %>%
  rename("year"=date, "debt"=DT.TDS.DECT.GN.ZS, 
         "NG Rents"=NY.GDP.NGAS.RT.ZS,"NR Rents"=NY.GDP.TOTL.RT.ZS)
# All data contained within 'main', the rest is superfluous
rm(ntg_data, ntr_data, dbt_data)

# Bring in ccodes, remove the country name 
# (removes any conflict with base_data country names)
main <- main %>%
  left_join(ccodes, by = c("country", "year")) %>%
  dplyr::select(-country) %>%
  distinct()

# Merge the variables with base_data
base_data <- base_data %>%
  left_join(main, by = c("ccode", "year"))
rm(main)

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.b.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  



