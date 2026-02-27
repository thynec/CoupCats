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
#setwd("~/Desktop/TEK 300") #Leah
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
# -----------------------------------------------------------------------------------------------#
#Bring in more modern GDP data - Tucker Working on
#---------------------
#insert World Bank GDP data; 
library(readxl)
df <- read_excel("~/Downloads/P_GDP Revised.xlsx")

df <- df %>%
  select(-'Time Code', -'GDP (constant 2015 US$) [NY.GDP.MKTP.KD]', -'Country Code') %>%
  rename(country = 'Country Name',
         year = Time,
         GDP = 'GDP (current US$) [NY.GDP.MKTP.CD]',
         GDP_Growth = 'GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]',
         GDP_percap = 'GDP per capita (current US$) [NY.GDP.PCAP.CD]',
         GDP_percapgr ='GDP per capita growth (annual %) [NY.GDP.PCAP.KD.ZG]') %>%
  arrange(country,year)

#adding in ccodes
library(readxl)
url <- "http://www.uky.edu/~clthyn2/replace_ccode_country.xls"
destfile <- "replace_ccode_country.xls"
curl::curl_download(url, destfile)
ccodes <- read_excel(destfile)
rm(destfile,url)

a <- df %>%
  left_join(ccodes, by =c("country" = "country", "year" = "year")) %>%
  relocate(ccode)
rm(df,ccodes)
#ALLGDP[ALLGDP == ".."] <- NA

a <- a %>%
  mutate(GDP = as.numeric(GDP),
         GDP_Growth = as.numeric(GDP_Growth),
         GDP_percap = as.numeric(GDP_percap),
         GDP_percapgr = as.numeric(GDP_percapgr)) %>%
  mutate(year = year+1) #lagged by 1 year 

base_data <- base_data %>%
  left_join(a, by = c("ccode"="ccode", "year"="year")) %>%
  select(-country.y) %>%
  rename(country = country.x)

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
rm(destfile, url)

#Cleaning up dataset
colnames(FP_CPI) <- FP_CPI[3, ]
FP_CPI <- FP_CPI[-c(1:3), ]
FP_CPI <- FP_CPI %>% #rearranging data to correct format
  pivot_longer(cols = `1960`:`2023`,  
               names_to = "Year",     
               values_to = "CPI")   
FP_CPI$Year <- as.numeric(FP_CPI$Year) #changing Year to numeric
FP_CPI <- FP_CPI %>% 
  select(`Country Name`, `Year`, `CPI`) %>% #deleting unnecessary columns
  mutate(Year=Year+1) %>% #Lag CPI
  rename(year = Year) %>%
  rename(country = `Country Name`) %>% 
  left_join(ccodes, by = c("year", "country")) %>% #merging in ccodes
  filter(!is.na(ccode))
FP_CPI <- FP_CPI %>% 
  select(-`country`)

FP_CPI$CPI <- as.numeric(as.character(FP_CPI$CPI))
FP_CPI_monthly <- FP_CPI %>% #expanding to monthly data
  uncount(weights = 12) %>%
  group_by(ccode, year) %>%
  mutate(month = 1:12) %>%
  ungroup()

FP_CPI_monthly <- FP_CPI_monthly %>% #Create Inflation
  group_by(ccode) %>%
  mutate(Inflation = (CPI - lag(CPI)) / lag(CPI) * 100) %>%
  select(-`CPI`)






#Bringing in IMF CPI - All measured using different base years
IMF_cpi <- read_csv("https://www.uky.edu/~clthyn2/IMF_cpi.csv")

#Cleaning dataset
IMF_cpi <- IMF_cpi %>%
  select(-matches("^(19[0-4][0-9]($|-M[0-9]{2}))")) %>% #removing data from 1900-1949
  select(-matches("^(19[0-4][0-9])-Q[1-4]$")) %>% #removing quarterly data prior to 1950
  filter(INDEX_TYPE != "Harmonised index of consumer prices (HICP)") %>% #obtaining only the CPI data
  select(4:8, 16:21, 46:1324) #removing unnecessary columns

IMF_cpi <- IMF_cpi %>%  
  filter(TYPE_OF_TRANSFORMATION == "Index") %>% #obtaining only index values
  filter(COICOP_1999 == "All Items") #only overall index and not individual goods

IMF_cpi <- IMF_cpi %>% 
  select(-`INDEX_TYPE`, -`COICOP_1999`, -`TYPE_OF_TRANSFORMATION`, -`OVERLAP`, -`STATUS`, -`IFS_FLAG`, -`DOI`)

#Separating data into yearly dataset
IMF_cpi_years <- IMF_cpi %>% #Separating yearly data to begin correctly merging data
  select(FREQUENCY, REFERENCE_PERIOD, COMMON_REFERENCE_PERIOD, COUNTRY, matches("^[0-9]{4}$"))  # Keeps only yearly columns

#Cleaning the main dataset for IMF to merge in CPI data
IMF_cpi <- IMF_cpi %>%
  select(COUNTRY) %>%
  distinct(COUNTRY, .keep_all = TRUE)
countries <- unique(IMF_cpi$COUNTRY)
IMF_cpi <- expand.grid(
  COUNTRY = countries,
  Year = 1950:2025,
  Month = 1:12
) %>%
  mutate(
    Quarter = case_when(
      Month %in% 1:3 ~ 1,
      Month %in% 4:6 ~ 2,
      Month %in% 7:9 ~ 3,
      Month %in% 10:12 ~ 4
    )
  )
IMF_cpi <- IMF_cpi %>%
  mutate(COUNTRY=as.character(COUNTRY))

IMF_cpi <- IMF_cpi %>%
  ungroup() %>%
  arrange(COUNTRY, Year, Month) %>%
  rename(year = Year) %>%
  rename(country=COUNTRY) %>%
  mutate(country=ifelse(country=="Congo, Dem. Rep.", "congo (drc)", country)) %>%
  mutate(country=ifelse(country=="Congo, Republic of", "congo (brazzaville)", country)) %>%
  mutate(country = gsub(",\\s*(Islamic|Democratic)?\\s*Republic of", "", country)) %>%
  mutate(country=ifelse(country=="Congo the", "Republic of Congo", country)) %>%
  mutate(country=ifelse(country=="Bahrain, Kingdom of", "Bahrain", country)) %>%
  mutate(country=ifelse(country=="Egypt, Arab Republic of", "Egypt", country)) %>%
  mutate(country=ifelse(country=="Ethiopia, The Federal Democratic Republic of", "Ethiopia", country)) %>%
  mutate(country=ifelse(country=="Lesotho, Kingdom of", "Lesotho", country)) %>%
  mutate(country=ifelse(country=="Netherlands, The", "Netherlands", country)) %>%
  mutate(country=ifelse(country=="Venezuela, República Bolivariana de", "Venezuela", country)) %>%
  left_join(ccodes, by = c("year", "country")) %>% #merging in ccodes
  filter(!is.na(ccode))

#Begin Work on Yearly sub-dataset
IMF_cpi_years <- IMF_cpi_years %>% #Cleaning Yearly data
  filter(FREQUENCY == "Annual") %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),  # Select only yearly columns (e.g., "1950")
    names_to = "Year",       # New column to store original year labels
    values_to = "CPI_Value"  # Store CPI values
  ) %>%
  mutate(
    Year = as.numeric(Year)  # Convert Year from character to numeric
  ) %>%
  select(FREQUENCY, REFERENCE_PERIOD, COMMON_REFERENCE_PERIOD, COUNTRY, Year, CPI_Value)
IMF_cpi_years <- IMF_cpi_years %>%
  rename(year = Year) %>%
  rename(country = `COUNTRY`) %>%
  mutate(country=ifelse(country=="Congo, Dem. Rep.", "congo (drc)", country)) %>%
  mutate(country=ifelse(country=="Congo, Republic of", "congo (brazzaville)", country)) %>%
  mutate(country = gsub(",\\s*(Islamic|Democratic)?\\s*Republic of", "", country)) %>%
  mutate(country=ifelse(country=="Congo the", "Republic of Congo", country)) %>%
  mutate(country=ifelse(country=="Bahrain, Kingdom of", "Bahrain", country)) %>%
  mutate(country=ifelse(country=="Egypt, Arab Republic of", "Egypt", country)) %>%
  mutate(country=ifelse(country=="Ethiopia, The Federal Democratic Republic of", "Ethiopia", country)) %>%
  mutate(country=ifelse(country=="Lesotho, Kingdom of", "Lesotho", country)) %>%
  mutate(country=ifelse(country=="Netherlands, The", "Netherlands", country)) %>%
  mutate(country=ifelse(country=="Venezuela, República Bolivariana de", "Venezuela", country)) %>%
  left_join(ccodes, by = c("year", "country")) %>% #merging in ccodes
  filter(!is.na(ccode))
IMF_cpi_years <- IMF_cpi_years %>% #rename variable as yearly cpi data
  rename(cpi_yearly = CPI_Value) 
IMF_cpi_years <- IMF_cpi_years %>%
  mutate(year=year+1) #Lag CPI
IMF_cpi_years <- IMF_cpi_years %>% 
  select(-`FREQUENCY`, -`REFERENCE_PERIOD`, -`COMMON_REFERENCE_PERIOD`, -`country`)
IMF_cpi <- IMF_cpi %>%  #merging yearly back into main data set
  left_join(IMF_cpi_years, by = c("ccode", "year"))


rm(IMF_cpi_years)

IMF_cpi <- IMF_cpi %>%
  rename(CPI = cpi_yearly)
IMF_cpi <- IMF_cpi %>%
  select(-'country', -'Quarter')  
IMF_cpi <- IMF_cpi %>%
  rename(month = Month)
IMF_cpi <- IMF_cpi %>% #Create Inflation
  group_by(ccode) %>%
  mutate(Inflation = (CPI - lag(CPI)) / lag(CPI) * 100) %>%
  select(-`CPI`)

#Bringing in UNdata for CPI
un <- read_csv("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/data/UNdata_Export_20250402_172632368.csv")

#Cleaning data
un <- un %>%
  select(-'OID', -'Magnitude') %>%
  rename(year = Year) %>%
  rename(country = 'Country or Area') 

un <- un %>% #Merging ccodes
  mutate(country = tolower(country)) %>%
  distinct()
ccodes <- ccodes %>%
  mutate(country = tolower(country)) %>%
  distinct() 
un <- un %>%
  mutate(country=ifelse(country=="congo, dem. rep. of", "democratic republic of congo", country)) %>%
  mutate(country=ifelse(country=="china,p.r.: mainland", "china", country)) %>%
  mutate(country=ifelse(country=="serbia, republic of", "serbia", country)) %>%
  mutate(country=ifelse(country=="venezuela, rep. bol.", "venezuela", country)) %>%
  mutate(country=ifelse(country=="azerbaijan, rep. of", "azerbaijan", country)) %>%
  mutate(country=ifelse(country=="bahrain, kingdom of", "bahrain", country)) %>%
  left_join(ccodes, by = c("country", "year"))
un <- un %>%
  filter(!is.na(ccode))
un <- un %>%
  filter(Description == "CPI % CHANGE") %>%
  select(`year`, `Value`, `ccode`) %>%
  rename(Inflation = Value) %>%
  mutate(year=year+1) #Lag CPI
#Expanding to monthly data
un <- un %>%
  uncount(12) %>%  # Expand each row into 12 rows (for each month)
  group_by(ccode, year) %>%
  mutate(month = 1:12,  # Assign month numbers 1 to 12 within each group
         Inflation = ifelse(month == 1, Inflation, 0)) %>%  # Keep inflation for month 1, zero otherwise
  ungroup()

#Merging CPI datasets
CPI <- IMF_cpi %>%
  left_join(FP_CPI_monthly, by = c("ccode", "year", "month"), suffix = c("", "_fp")) %>%
  mutate(Inflation = coalesce(Inflation, Inflation_fp)) %>%  # Fill in missing Inflation
  select(-Inflation_fp)

CPI <- CPI %>%
  left_join(un, by = c("ccode", "year", "month"), suffix = c("", "_un")) %>%
  mutate(Inflation = coalesce(Inflation, Inflation_un)) %>%  # Fill in any remaining missing Inflation
  select(-Inflation_un)
rm(FP_CPI)
rm(FP_CPI_monthly)
rm(IMF_cpi)
rm(un)

#running out of time; just merge by year and deal with monthly variations later
CPI_yearly <- CPI %>%
  filter(month==1) %>%
  select(-month) %>%
  distinct() #note that obs drop because ccode=484 has duplicates; nothing hard by dropping these, though

base_data <- base_data %>%
  left_join(CPI_yearly, by=c("ccode", "year"))

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.b.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  

#------------------------------------------------------------------------------------------------#      
# VARIABLES PULLED FROM WORLD BANK: RESOURCE RENTS, DEBT, TOURISM, GINI
#------------------------------------------------------------------------------------------------# 

# Bringing in data. 
world_bank <- WDI(country = "all",
                  indicator = c("DT.ODA.ODAT.GN.ZS", "NY.GDP.NGAS.RT.ZS", "NY.GDP.TOTL.RT.ZS",
                                "DT.TDS.DECT.GN.ZS", "ST.INT.RCPT.CD", "ST.INT.XPND.CD", 
                                "SI.POV.GINI"),
                  start = 1960,
                  end = 2025,
                  extra = TRUE)

# Cleaning up data. 
world_bank <- world_bank %>% 
  select(country, year, DT.ODA.ODAT.GN.ZS, NY.GDP.NGAS.RT.ZS, NY.GDP.TOTL.RT.ZS, DT.TDS.DECT.GN.ZS, ST.INT.RCPT.CD, ST.INT.XPND.CD, SI.POV.GINI) %>%
  rename(oda = DT.ODA.ODAT.GN.ZS, 
         ngas = NY.GDP.NGAS.RT.ZS,
         nr_rents = NY.GDP.TOTL.RT.ZS, 
         debt = DT.TDS.DECT.GN.ZS, 
         trsm_inflows = ST.INT.RCPT.CD, 
         trsm_outflows = ST.INT.XPND.CD, 
         gini = SI.POV.GINI) %>% 
  left_join(ccodes, by = c("country", "year")) %>%
  select(-country)

# Merging into base data. 
base_data <- base_data %>%
  left_join(world_bank, by = c("ccode", "year"))
rm(world_bank)


