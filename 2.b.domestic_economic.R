#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building domestic economic variables

#1. clear all
rm(list = ls())
#2. set working directory
#setwd("~/R/coupcats") # Set working file. 
setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/teaching/1.coupcast/TEK_S26/git_2026.03.13") #Clay at home
#setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/teaching/1.coupcast/TEK_S26/git_2026.03.13") #clay at work
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
         c_merge = country) %>%
  mutate(month=12)
base_data <- base_data %>%
  left_join(vdem_gdppc, by = c("ccode", "year", "month"))
base_data <- base_data %>%
  subset(select = -c(c_merge))
rm(vdem_gdppc, vdem)

#Penn World Table GDP Data
pwt <- read_stata("https://dataverse.nl/api/access/datafile/554030")

pwt <- pwt %>%
  select(country, year, pop, hc, rgdpna) %>%
  set_variable_labels(
    pop="pop, Penn, t-1, ln",
    hc="human cap index, Penn, t-1"
  ) %>%
  mutate(year=year+1) %>%
  mutate(gdppc=log10(rgdpna/pop)) %>%
  select(-rgdpna, -pop) %>% 
  left_join(ccodes, by = c("country" = "country", "year" = "year"))
check <- pwt %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct()
rm(check)
#need to add:
#490 = D.R. of the Congo
#812 = Lao People's DR
#510 = U.R. of Tanzania: Mainland
pwt <- pwt %>%
  mutate(ccode=ifelse(country=="D.R. of the Congo", 490, ccode)) %>%
  mutate(ccode=ifelse(country=="Lao People's DR", 812, ccode)) %>%
  mutate(ccode=ifelse(country=="U.R. of Tanzania: Mainland", 510, ccode)) %>%
  select(-country) %>%
  mutate(month=12)
base_data <- base_data %>%
  left_join(pwt, by = c("ccode", "year", "month"))
rm(pwt)

#WDI
wdi_gdppc <- WDI(country = "all",
                 indicator = "NY.GDP.PCAP.CD", 
                 start = 1960, 
                 end = 2026,
                 extra = TRUE)
wdi_gdppc <- wdi_gdppc %>%
  subset(select = c(country, year, NY.GDP.PCAP.CD)) %>%
  rename(wdi_gdppc = NY.GDP.PCAP.CD) %>%
  mutate(year=year+1) %>% #just lagged
  mutate(month=12) %>%
  left_join(ccodes, by = c("year", "country")) %>%
  rename(c_merge = country) %>%
  mutate(ccode=ifelse(c_merge=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(c_merge=="Somalia, Fed. Rep.", 520, ccode)) %>%
  select(-c_merge)
base_data <- base_data %>%
  left_join(wdi_gdppc, by = c("ccode", "year", "month")) 
rm(wdi_gdppc)
cor.test(base_data$gdppc, base_data$vdem_gdppc) #looks reasonable
cor.test(base_data$gdppc, base_data$wdi_gdppc) #looks reasonable
cor.test(base_data$vdem_gdppc, base_data$wdi_gdppc) #looks reasonable

#year, state coverage...
penn <- base_data %>%
  select(year, gdppc) %>%
  filter(!is.na(gdppc))
summary(penn) #1951-2024
rm(penn)
vdem <- base_data %>%
  select(year, vdem_gdppc) %>%
  filter(!is.na(vdem_gdppc))
summary(vdem) #1950-2020
rm(vdem)
wdi <- base_data %>%
  select(year, wdi_gdppc) %>%
  filter(!is.na(wdi_gdppc))
summary(wdi) #1961-2025
rm(wdi)

#not seeing Penn add anything, so going to drop that. It also has the lowest correlation.
#linear interpolate both WDI and Vdem
base_data <- base_data %>%
  select(-gdppc) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(vdem_gdppc_OG=vdem_gdppc) %>%
  set_variable_labels(vdem_gdppc_OG = "vdem, original, gdppc, t-1") %>%
  mutate(wdi_gdppc_OG=wdi_gdppc) %>%
  set_variable_labels(wdi_gdppc_OG = "wdi, original, gdppc, t-1") %>%
  mutate(
    time_id = year + (month - 1) / 12,
    vdem = na.approx(vdem_gdppc, x = time_id, na.rm = FALSE)
  ) %>%
  mutate(wdi=na.approx(wdi_gdppc, x=time_id, na.rm=FALSE)) %>%
  ungroup() %>%
  select(-vdem_gdppc, -wdi_gdppc)
cor.test(base_data$vdem, base_data$wdi) #looks reasonable
base_data <- base_data %>%
  group_by(ccode) %>%
  arrange(ccode, year, month) %>%
  mutate(per = (wdi - lag(wdi)) / lag(wdi)) %>%
  mutate(
    gdppc = accumulate(
      seq_along(vdem),
      .init = vdem[1],
      ~ if (!is.na(vdem[.y])) {
        vdem[.y]
      } else {
        .x * (1 + per[.y])
      }
    )[-1]
  ) %>%
  ungroup()

lin_extrap <- function(x, y) {
  keep <- !is.na(y)
  
  if (sum(keep) == 0) return(y)
  if (sum(keep) == 1) return(rep(y[keep][1], length(y)))
  
  x_obs <- x[keep]
  y_obs <- y[keep]
  
  y_new <- approx(x_obs, y_obs, xout = x, method = "linear", rule = 1)$y
  
  left_slope <- (y_obs[2] - y_obs[1]) / (x_obs[2] - x_obs[1])
  left_idx <- x < min(x_obs)
  y_new[left_idx] <- y_obs[1] + left_slope * (x[left_idx] - x_obs[1])
  
  n <- length(x_obs)
  right_slope <- (y_obs[n] - y_obs[n - 1]) / (x_obs[n] - x_obs[n - 1])
  right_idx <- x > max(x_obs)
  y_new[right_idx] <- y_obs[n] + right_slope * (x[right_idx] - x_obs[n])
  
  y_new
}
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    gdppc = lin_extrap(time_id, gdppc)
  ) %>%
  ungroup() %>%
  mutate(gdppc=log10(gdppc+1))
base_data <- base_data %>%
  select(-vdem, -wdi, -per) %>%
  set_variable_labels(gdppc = "GDP/cap, WDI+vdem splice, interpolated")

#deal with human cap index from Penn
summary(base_data$hc) #range: 1.007, 3.986
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  mutate(hc_OG = hc) %>%
  group_by(ccode) %>%
  mutate(hc = lin_extrap(time_id, hc_OG)) %>%
  mutate(hc = ifelse(hc<1.007, 1.007, hc)) %>%
  mutate(hc = ifelse(hc>3.986, 3.986, hc)) %>%
  ungroup() %>%
  set_variable_labels(hc_OG = "human cap index, original, Penn, t-1") %>%
  set_variable_labels(hc = "human cap index, interpolated")

#create % change in GDP/cap
base_data <- base_data %>%
  group_by(ccode) %>%
  mutate(ch_gdppc=((gdppc-lag(gdppc))/lag(gdppc))) %>%
  set_variable_labels(ch_gdppc = "% ch in ln GDP/cap")

#------------------------------------------------------------------------------------------------#      
# VARIABLES PULLED FROM WORLD BANK: RESOURCE RENTS, DEBT, TOURISM, GINI
#looking at data, removed tourism - just not enough data
#------------------------------------------------------------------------------------------------# 

# Bringing in data. 
wb <- WDI(country = "all",
                  indicator = c("DT.ODA.ODAT.GN.ZS", "NY.GDP.NGAS.RT.ZS", "NY.GDP.TOTL.RT.ZS",
                                "DT.TDS.DECT.GN.ZS", "ST.INT.RCPT.CD", "ST.INT.XPND.CD", 
                                "SI.POV.GINI"),
                  start = 1960,
                  end = 2026,
                  extra = TRUE)

# Cleaning up data. 
wb <- wb %>% 
  select(country, year, DT.ODA.ODAT.GN.ZS, NY.GDP.NGAS.RT.ZS, NY.GDP.TOTL.RT.ZS, DT.TDS.DECT.GN.ZS, ST.INT.RCPT.CD, ST.INT.XPND.CD, SI.POV.GINI) %>%
  rename(oda = DT.ODA.ODAT.GN.ZS, 
         ngas = NY.GDP.NGAS.RT.ZS,
         nr_rents = NY.GDP.TOTL.RT.ZS, 
         debt = DT.TDS.DECT.GN.ZS, 
         trsm_inflows = ST.INT.RCPT.CD, 
         trsm_outflows = ST.INT.XPND.CD, 
         gini = SI.POV.GINI) %>% 
  left_join(ccodes, by = c("country", "year")) %>%
  select(-trsm_inflows, -trsm_outflows, -ngas)
check <- wb %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct()
rm(check)
wb <- wb %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(country=="Somalia, Fed. Rep.", 520, ccode)) %>%
  relocate(country, ccode, year) %>%
  mutate(month=12) %>%
  mutate(year=year+1) %>% #just lagged
  select(-country)

# Merging into base data. 
base_data <- base_data %>%
  left_join(wb, by = c("ccode", "year", "month"))
rm(wb)

#deal with missing data
summary(base_data$oda) #oda range: -8.224, 169.593
oda <- base_data %>% 
    filter(!is.na(oda))
  summary(oda$year) #oda = 1961-2024; assume na=0
  rm(oda)
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(oda_OG = oda) %>%
  set_variable_labels(oda_OG = "Net ODA received (% of GNI), original, t-1") %>%
  mutate(
    oda = if_else(
      year < 1961,
      oda,
      lin_extrap(row_number(), oda)
    )
  ) %>%
  mutate(oda=ifelse(is.na(oda) & year>=1961, 0, oda)) %>%
  mutate(oda=ifelse(oda<0, -8.224, oda)) %>%
  mutate(oda=ifelse(oda>169.593, 169.593, oda))

summary(base_data$nr_rents) #nr_rents range: 0, 88.592
nr_rents <- base_data %>% 
  filter(!is.na(nr_rents))
    summary(nr_rents$year) #nr_rents = 1971-2022
    rm(nr_rents)
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(nr_rents_OG = nr_rents) %>%
  set_variable_labels(nr_rents_OG = "Nat resource rents (% of GDP), original, t-1") %>%
  mutate(
    nr_rents = if_else(
      year < 1971,
      nr_rents,
      lin_extrap(row_number(), nr_rents)
    )
  ) %>%
  mutate(nr_rents=ifelse(is.na(nr_rents) & year>=1971, 0, nr_rents)) %>%
  mutate(nr_rents=ifelse(nr_rents<0, 0, nr_rents)) %>%
  mutate(nr_rents=ifelse(nr_rents>88.592, 88.592, nr_rents))
    
summary(base_data$debt) #debt range: 0, 102.222
debt <- base_data %>% filter(!is.na(debt))
  summary(debt$year) #debt = 1971-2025
    rm(debt)
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(debt_OG=debt) %>%
  set_variable_labels(debt_OG = "Total debt svc (% of GNI), original, t-1") %>%
  mutate(
    debt = if_else(
      year < 1971,
      debt,
      lin_extrap(row_number(), debt)
    )
  ) %>%
  mutate(debt=ifelse(is.na(debt) & year>=1971, 0, debt)) %>%
  mutate(debt=ifelse(debt<0, 0, debt)) %>%
  mutate(debt=ifelse(debt>102.222, 102.222, debt))

base_data <- base_data %>%
  relocate(country, ccode, year, month, coup_attempt, gdppc, hc, oda, nr_rents, debt, gini)
    
#------------------------------------------------#
#Work on GINI; splice WDI/WIID/SWIID
#------------------------------------------------#

#WDI stuff
summary(base_data$gini) #range: 20.2, 71.1
gini <- base_data %>% 
  filter(!is.na(gini))
summary(gini$year) #debt = 1964-2025
rm(gini)

#Pulling Gini from UN WIID
url <- "https://www.wider.unu.edu/sites/default/files/WIID/wiidcountry_4.xlsx"
destfile <- "wiidcountry_4.xlsx"
curl::curl_download(url, destfile)
wiid <- read_excel(destfile)
rm(destfile, url)
  
wiid <- wiid %>%
  arrange(country, year)
wiid <- wiid %>%
  filter(giniseries == 1) %>%
  select(country,year,gini_std) %>%
  rename(gini_wiid = gini_std) %>%
  left_join(ccodes, by = c("country" = "country", "year" = "year")) 
check <- wiid %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct()
#need to add...
wiid <- wiid %>%
  mutate(ccode=ifelse(country=="Congo, Democratic Republic of the", 490, ccode)) %>%
  mutate(ccode=ifelse(country=="Congo, Republic of the", 484, ccode)) %>%
  mutate(ccode=ifelse(country=="Japan", 740, ccode)) %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(country=="United States", 2, ccode)) 
check <- wiid %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct() #we're good
rm(check)
wiid <- wiid %>%
  mutate(year=year+1) %>% #just lagged
  mutate(month=12) %>%
  select(-country) %>%
  group_by(ccode, year, month) %>%
  distinct() %>%
  ungroup()
check <- wiid %>%
  arrange(ccode, year, month) %>%
  mutate(check=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) #this is Russia; neither duplicate makes a ton of sense; drop both to be safe
rm(check)
wiid <- wiid %>%
  mutate(gini_wiid=ifelse(ccode==365 & year==1994, -999, gini_wiid)) %>%
  filter(!is.na(ccode)) %>%
  filter(gini_wiid>=0)
base_data <- base_data %>%
  left_join(wiid, by = c("ccode", "year", "month")) %>%
  set_variable_labels(gini_wiid = "GINI; WIID, original, t-1") %>%
  rename(gini_wiid_OG=gini_wiid) %>%
  rename(gini_wdi_OG=gini) %>%
  set_variable_labels(gini_wdi_OG = "GINI, WDI, original, t-1") %>%
  relocate(gini_wdi_OG, gini_wiid_OG)
rm(wiid)

#pulling Gini from SWIID
swiid <- read_csv("https://github.com/fsolt/swiid/raw/master/data/swiid_summary.csv")
swiid <- swiid %>%
  select(country,year,gini_disp) %>%
  mutate(year=year+1) %>%
  left_join(ccodes, by = c("country", "year"))
summary(swiid) #1961-2025; range: 16.9 - 65.2
check <- swiid %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct() #need to add: Congo-Kinshasa = 490
swiid <- swiid %>%
  mutate(ccode=ifelse(country=="Congo-Kinshasa", 490, ccode))
rm(check)
swiid <- swiid %>%
  select(-country) %>%
  rename(gini_swiid_OG=gini_disp) %>%
  set_variable_labels(gini_swiid_OG = "GINI, SWIID, original, t-1") %>%
  mutate(month=12)
swiid <- swiid %>%
  arrange(ccode, year, month) %>%
  mutate(check=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) %>% #these are just Russia duplicates; can drop
  filter(check!=1) %>%
  select(-check) 

base_data <- base_data %>%
  left_join(swiid, by = c("ccode", "year", "month")) %>%
  relocate(gini_wdi_OG, gini_wiid_OG, gini_swiid_OG)
rm(swiid)

cor.test(base_data$gini_wdi_OG, base_data$gini_wiid_OG) 
cor.test(base_data$gini_wdi_OG, base_data$gini_swiid_OG)
cor.test(base_data$gini_wiid_OG, base_data$gini_swiid_OG) 
  #...getting at the same thing

base_data <- base_data %>%
  ungroup() %>%
  mutate(gini_wdi_OG = as.vector(scale(gini_wdi_OG))) %>%
  mutate(gini_wiid_OG = as.vector(scale(gini_wiid_OG))) %>%
  mutate(gini_swiid_OG = as.vector(scale(gini_swiid_OG))) %>%
  mutate(gini=gini_swiid_OG) %>%
  mutate(gini=ifelse(is.na(gini), gini_wiid_OG, gini)) %>%
  mutate(gini=ifelse(is.na(gini), gini_wdi_OG, gini)) %>%
  relocate(gini, gini_wdi_OG, gini_wiid_OG, gini_swiid_OG)

summary(base_data$gini) #range: -2.464, 3.438
gini <- base_data %>%
  filter(!is.na(gini))
summary(gini$year) #years: 1950-2025
rm(gini)
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(gini_OG = gini) %>%
  set_variable_labels(gini_OG = "gini, original, t-1, swiid wiid wdi") %>%
  mutate(gini=lin_extrap(row_number(), gini_OG)) %>%
  mutate(gini=ifelse(gini < -2.464, -2.464, gini)) %>%
  mutate(gini=ifelse(gini > 3.438, 3.438, gini))
  
base_data <- base_data %>%
  relocate(country, ccode, year, month, coup_attempt, pce, pce2, pce3, gdppc, hc, oda, nr_rents, debt, gini) %>%
  select(-time_id)
rm(lin_extrap)

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.b.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  


#
#
#
#
#
#
##------------------------------------------------------------------------------------------------#      
##building CPI; as of 03/28/26 clay didn't update anything below for CPI
##------------------------------------------------------------------------------------------------# 
#
##Bringing in CPI data from world bank, using 2010 as base year
#url <- "https://api.worldbank.org/v2/en/indicator/FP.CPI.TOTL?downloadformat=excel"
#destfile <- "FP_CPI.xls"
#curl::curl_download(url, destfile)
#FP_CPI <- read_excel(destfile)
#rm(destfile, url)
#
##Cleaning up dataset
#colnames(FP_CPI) <- FP_CPI[3, ]
#FP_CPI <- FP_CPI[-c(1:3), ]
#FP_CPI <- FP_CPI %>% #rearranging data to correct format
#  pivot_longer(cols = `1960`:`2023`,  
#               names_to = "Year",     
#               values_to = "CPI")   
#FP_CPI$Year <- as.numeric(FP_CPI$Year) #changing Year to numeric
#FP_CPI <- FP_CPI %>% 
#  select(`Country Name`, `Year`, `CPI`) %>% #deleting unnecessary columns
#  mutate(Year=Year+1) %>% #Lag CPI
#  rename(year = Year) %>%
#  rename(country = `Country Name`) %>% 
#  left_join(ccodes, by = c("year", "country")) %>% #merging in ccodes
#  filter(!is.na(ccode))
#FP_CPI <- FP_CPI %>% 
#  select(-`country`)
#
#FP_CPI$CPI <- as.numeric(as.character(FP_CPI$CPI))
#FP_CPI_monthly <- FP_CPI %>% #expanding to monthly data
#  uncount(weights = 12) %>%
#  group_by(ccode, year) %>%
#  mutate(month = 1:12) %>%
#  ungroup()
#
#FP_CPI_monthly <- FP_CPI_monthly %>% #Create Inflation
#  group_by(ccode) %>%
#  mutate(Inflation = (CPI - lag(CPI)) / lag(CPI) * 100) %>%
#  select(-`CPI`)
#
#
#
#
#
#
##Bringing in IMF CPI - All measured using different base years
#IMF_cpi <- read_csv("https://www.uky.edu/~clthyn2/IMF_cpi.csv")
#
##Cleaning dataset
#IMF_cpi <- IMF_cpi %>%
#  select(-matches("^(19[0-4][0-9]($|-M[0-9]{2}))")) %>% #removing data from 1900-1949
#  select(-matches("^(19[0-4][0-9])-Q[1-4]$")) %>% #removing quarterly data prior to 1950
#  filter(INDEX_TYPE != "Harmonised index of consumer prices (HICP)") %>% #obtaining only the CPI data
#  select(4:8, 16:21, 46:1324) #removing unnecessary columns
#
#IMF_cpi <- IMF_cpi %>%  
#  filter(TYPE_OF_TRANSFORMATION == "Index") %>% #obtaining only index values
#  filter(COICOP_1999 == "All Items") #only overall index and not individual goods
#
#IMF_cpi <- IMF_cpi %>% 
#  select(-`INDEX_TYPE`, -`COICOP_1999`, -`TYPE_OF_TRANSFORMATION`, -`OVERLAP`, -`STATUS`, -`IFS_FLAG`, -`DOI`)
#
##Separating data into yearly dataset
#IMF_cpi_years <- IMF_cpi %>% #Separating yearly data to begin correctly merging data
#  select(FREQUENCY, REFERENCE_PERIOD, COMMON_REFERENCE_PERIOD, COUNTRY, matches("^[0-9]{4}$"))  # Keeps only yearly columns
#
##Cleaning the main dataset for IMF to merge in CPI data
#IMF_cpi <- IMF_cpi %>%
#  select(COUNTRY) %>%
#  distinct(COUNTRY, .keep_all = TRUE)
#countries <- unique(IMF_cpi$COUNTRY)
#IMF_cpi <- expand.grid(
#  COUNTRY = countries,
#  Year = 1950:2025,
#  Month = 1:12
#) %>%
#  mutate(
#    Quarter = case_when(
#      Month %in% 1:3 ~ 1,
#      Month %in% 4:6 ~ 2,
#      Month %in% 7:9 ~ 3,
#      Month %in% 10:12 ~ 4
#    )
#  )
#IMF_cpi <- IMF_cpi %>%
#  mutate(COUNTRY=as.character(COUNTRY))
#
#IMF_cpi <- IMF_cpi %>%
#  ungroup() %>%
#  arrange(COUNTRY, Year, Month) %>%
#  rename(year = Year) %>%
#  rename(country=COUNTRY) %>%
#  mutate(country=ifelse(country=="Congo, Dem. Rep.", "congo (drc)", country)) %>%
#  mutate(country=ifelse(country=="Congo, Republic of", "congo (brazzaville)", country)) %>%
#  mutate(country = gsub(",\\s*(Islamic|Democratic)?\\s*Republic of", "", country)) %>%
#  mutate(country=ifelse(country=="Congo the", "Republic of Congo", country)) %>%
#  mutate(country=ifelse(country=="Bahrain, Kingdom of", "Bahrain", country)) %>%
#  mutate(country=ifelse(country=="Egypt, Arab Republic of", "Egypt", country)) %>%
#  mutate(country=ifelse(country=="Ethiopia, The Federal Democratic Republic of", "Ethiopia", country)) %>%
#  mutate(country=ifelse(country=="Lesotho, Kingdom of", "Lesotho", country)) %>%
#  mutate(country=ifelse(country=="Netherlands, The", "Netherlands", country)) %>%
#  mutate(country=ifelse(country=="Venezuela, República Bolivariana de", "Venezuela", country)) %>%
#  left_join(ccodes, by = c("year", "country")) %>% #merging in ccodes
#  filter(!is.na(ccode))
#
##Begin Work on Yearly sub-dataset
#IMF_cpi_years <- IMF_cpi_years %>% #Cleaning Yearly data
#  filter(FREQUENCY == "Annual") %>%
#  pivot_longer(
#    cols = matches("^[0-9]{4}$"),  # Select only yearly columns (e.g., "1950")
#    names_to = "Year",       # New column to store original year labels
#    values_to = "CPI_Value"  # Store CPI values
#  ) %>%
#  mutate(
#    Year = as.numeric(Year)  # Convert Year from character to numeric
#  ) %>%
#  select(FREQUENCY, REFERENCE_PERIOD, COMMON_REFERENCE_PERIOD, COUNTRY, Year, CPI_Value)
#IMF_cpi_years <- IMF_cpi_years %>%
#  rename(year = Year) %>%
#  rename(country = `COUNTRY`) %>%
#  mutate(country=ifelse(country=="Congo, Dem. Rep.", "congo (drc)", country)) %>%
#  mutate(country=ifelse(country=="Congo, Republic of", "congo (brazzaville)", country)) %>%
#  mutate(country = gsub(",\\s*(Islamic|Democratic)?\\s*Republic of", "", country)) %>%
#  mutate(country=ifelse(country=="Congo the", "Republic of Congo", country)) %>%
#  mutate(country=ifelse(country=="Bahrain, Kingdom of", "Bahrain", country)) %>%
#  mutate(country=ifelse(country=="Egypt, Arab Republic of", "Egypt", country)) %>%
#  mutate(country=ifelse(country=="Ethiopia, The Federal Democratic Republic of", "Ethiopia", country)) %>%
#  mutate(country=ifelse(country=="Lesotho, Kingdom of", "Lesotho", country)) %>%
#  mutate(country=ifelse(country=="Netherlands, The", "Netherlands", country)) %>%
#  mutate(country=ifelse(country=="Venezuela, República Bolivariana de", "Venezuela", country)) %>%
#  left_join(ccodes, by = c("year", "country")) %>% #merging in ccodes
#  filter(!is.na(ccode))
#IMF_cpi_years <- IMF_cpi_years %>% #rename variable as yearly cpi data
#  rename(cpi_yearly = CPI_Value) 
#IMF_cpi_years <- IMF_cpi_years %>%
#  mutate(year=year+1) #Lag CPI
#IMF_cpi_years <- IMF_cpi_years %>% 
#  select(-`FREQUENCY`, -`REFERENCE_PERIOD`, -`COMMON_REFERENCE_PERIOD`, -`country`)
#IMF_cpi <- IMF_cpi %>%  #merging yearly back into main data set
#  left_join(IMF_cpi_years, by = c("ccode", "year"))
#
#
#rm(IMF_cpi_years)
#
#IMF_cpi <- IMF_cpi %>%
#  rename(CPI = cpi_yearly)
#IMF_cpi <- IMF_cpi %>%
#  select(-'country', -'Quarter')  
#IMF_cpi <- IMF_cpi %>%
#  rename(month = Month)
#IMF_cpi <- IMF_cpi %>% #Create Inflation
#  group_by(ccode) %>%
#  mutate(Inflation = (CPI - lag(CPI)) / lag(CPI) * 100) %>%
#  select(-`CPI`)
#
##Bringing in UNdata for CPI
#un <- read_csv("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/data/UNdata_Export_20250402_172632368.csv")
#
##Cleaning data
#un <- un %>%
#  select(-'OID', -'Magnitude') %>%
#  rename(year = Year) %>%
#  rename(country = 'Country or Area') 
#
#un <- un %>% #Merging ccodes
#  mutate(country = tolower(country)) %>%
#  distinct()
#ccodes_lower <- ccodes %>%
#  mutate(country = tolower(country)) %>%
#  distinct() 
#un <- un %>%
#  mutate(country=ifelse(country=="congo, dem. rep. of", "democratic republic of congo", country)) %>%
#  mutate(country=ifelse(country=="china,p.r.: mainland", "china", country)) %>%
#  mutate(country=ifelse(country=="serbia, republic of", "serbia", country)) %>%
#  mutate(country=ifelse(country=="venezuela, rep. bol.", "venezuela", country)) %>%
#  mutate(country=ifelse(country=="azerbaijan, rep. of", "azerbaijan", country)) %>%
#  mutate(country=ifelse(country=="bahrain, kingdom of", "bahrain", country)) %>%
#  left_join(ccodes_lower, by = c("country", "year"))
#rm(ccodes_lower)
#un <- un %>%
#  filter(!is.na(ccode))
#un <- un %>%
#  filter(Description == "CPI % CHANGE") %>%
#  select(`year`, `Value`, `ccode`) %>%
#  rename(Inflation = Value) %>%
#  mutate(year=year+1) #Lag CPI
##Expanding to monthly data
#un <- un %>%
#  uncount(12) %>%  # Expand each row into 12 rows (for each month)
#  group_by(ccode, year) %>%
#  mutate(month = 1:12,  # Assign month numbers 1 to 12 within each group
#         Inflation = ifelse(month == 1, Inflation, 0)) %>%  # Keep inflation for month 1, zero otherwise
#  ungroup()
#
##Merging CPI datasets
#CPI <- IMF_cpi %>%
#  left_join(FP_CPI_monthly, by = c("ccode", "year", "month"), suffix = c("", "_fp")) %>%
#  mutate(Inflation = coalesce(Inflation, Inflation_fp)) %>%  # Fill in missing Inflation
#  select(-Inflation_fp)
#
#CPI <- CPI %>%
#  left_join(un, by = c("ccode", "year", "month"), suffix = c("", "_un")) %>%
#  mutate(Inflation = coalesce(Inflation, Inflation_un)) %>%  # Fill in any remaining missing Inflation
#  select(-Inflation_un)
#rm(FP_CPI)
#rm(FP_CPI_monthly)
#rm(IMF_cpi)
#rm(un)
#
##running out of time; just merge by year and deal with monthly variations later
#CPI_yearly <- CPI %>%
#  filter(month==1) %>%
#  select(-month) %>%
#  distinct() #note that obs drop because ccode=484 has duplicates; nothing hard by dropping these, though
#
#base_data <- base_data %>%
#  left_join(CPI_yearly, by=c("ccode", "year"))
#
#
#
#
#
##-------------------------------------------------------------------------------------#
##   Bringing in ECI and KOF data sets 
##-------------------------------------------------------------------------------------#
##"Growth Lab and Complexity Rankings" 
#
#read.csv("C:/Users/catal/OneDrive/Desktop/coupcats/growth_proj_eci_rankings.csv")
#
##eci_data <- read_csv(file.choose()) 
#
#install.packages("countrycode")
#library(countrycode) 
#
#eci_data2 <- eci_data2 %>%
#  mutate(country = countrycode(country_iso3_code,
#                               origin = "iso3c",
#                               destination = "country.name"))
#rm(eci_data2)
#
#eci_data2b <- eci_data2 %>% #main dataset to use
#  select(
#    country, 
#    eci_hs92, 
#    eci_rank_hs92, 
#    year) %>%
#  mutate(month = list(1:12)) %>% #expand to monthly 
#  unnest(month) 
#
#base_data <- base_data %>% #merging base data with eci data 
#  left_join(eci_data2b %>%
#              select(country, 
#                     year, 
#                     month, 
#                     eci_hs92), 
#            by = c("country", 
#                   "year", 
#                   "month"))  
#
#
#
#
