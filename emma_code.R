rm(list = ls())

setwd("~/R/coupcats") # Set working file. 

# source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 

source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/step1.R")

# -------------------------- V-Dem Data ------------------------------ #

# V-Dem data. 
# Reading in data. 
vdem <- vdem 

'The V-Dem dataset does not cover some countries, namely: 
  Andorra, Antigua and Barbuda, Bahamas, Belize, Brunei, Dominica, 
  Federated States of Micronesia, Grenada, Kiribati, Liechtenstein, 
  Marshall Islands, Monaco, Nauru, Palau, Saint Kitts and Nevis, 
  Saint Lucia, Saint Vincent and the Grenadines, Samoa, San Marino, 
  Tonga, Tuvalu, and the Vatican.'

# Cleaning up data. 
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
  filter(year >= 1950)
vdem <- vdem %>% # Merging in ccodes. 
  left_join(ccodes, by = c("year", "country")) %>%
  filter(!is.na(ccode)) # Non-state actors. 

# GDPPC (e_gdppc). 
# Cleaning data. 
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
rm(vdem_gdppc)

# Reading in WDI. 
wdi_gdppc <- WDI(country = "all",
                 indicator = "NY.GDP.PCAP.CD", 
                 start = 1960, 
                 end = 2024,
                 extra = TRUE)
wdi_gdppc <- wdi_gdppc %>%
  subset(select = c(country, year, NY.GDP.PCAP.CD)) %>%
  rename(wdi_gdppc = NY.GDP.PCAP.CD) %>%
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

# 1. Regime type (v2x_polyarchy). 
vdem_regime2 <- vdem %>% 
  subset(select = c(country, ccode, year, regime2)) %>% 
  filter(!(country == "Kazakhstan" & year == 1990)) %>% # Kazakhstan became independent this year.
  filter(!(country == "Turkmenistan" & year == 1990)) %>% # Turkmenistan became independent this year. 
  subset(select = -c(country))
base_data <- base_data %>% 
  left_join(vdem_regime2, by = c("ccode", "year")) %>%
  filter(!(ccode %in% c(232, 58, 31, 80, 835, 54, 987, 55, 946, 223, 
                        983, 221, 970, 986, 60, 56, 57, 990, 331, 955,
                        947))) %>%
  filter(!(ccode == "471" & year == 1960)) %>% # Cameroon became independent this year. 
  filter(!(ccode == "678" & year <= 1991)) %>% # Yemen unified in 1991. 
  filter(year < 2024) 
base_data <- base_data %>% # Ultimately, we are losing Czechoslovakia and the German Federal Republic. 
  filter(!(ccode == "260")) %>%
  filter(!(ccode == "315"))
rm(vdem_regime2)

# Interpolating by WDI. 
base_data <- base_data %>%
  select(country, ccode, year, vdem_gdppc, wdi_gdppc) %>%
  distinct() %>%
  mutate(change=(wdi_gdppc-lag(wdi_gdppc))) %>%
  mutate(now=vdem_gdppc) %>%
  mutate(perc_change=change/lag(wdi_gdppc)) %>%
  mutate(gdppc=ifelse(is.na(now), lag(now)*perc_change+lag(vdem_gdppc), now)) %>%
  mutate(gdppc=ifelse(is.na(gdppc), lag(gdppc)*perc_change+lag(gdppc), gdppc)) %>%
  mutate(gdppc=ifelse(is.na(gdppc), lag(gdppc)*perc_change+lag(gdppc), gdppc)) %>%
  subset(select = -c(wdi_gdppc, vdem_gdppc, change, now, perc_change))

# 2. Regime type (v2x_regime). 
vdem_regime <- vdem %>% 
  subset(select = c(country, ccode, year, regime)) %>%
  filter(!(country == "Kazakhstan" & year == 1990)) %>% 
  filter(!(country == "Turkmenistan" & year == 1990)) %>% 
  subset(select = -c(country))
base_data <- base_data %>% 
  left_join(vdem_regime, by = c("ccode", "year")) # No duplicates. 
rm(vdem_regime)

# 3. Executive corruption index (v2x_execorr).
vdem_execorr <- vdem %>% 
  subset(select = c(country, ccode, year, exec_corr)) %>%
  filter(!(ccode == "860" & year < 2001)) %>% # Timor-Leste gained independence in 2000. 
  subset(select = -c(country))
base_data <- base_data %>% 
  left_join(vdem_execorr, by = c("ccode", "year")) 
rm(vdem_execorr)

# 4. Judicial constraints on the executive index (v2x_jucon)
vdem_judconst <- vdem %>% 
  subset(select = c(country, ccode, year, jud_const)) %>%
  filter(!(ccode == "860" & year < 2001)) %>% 
  subset(select = -c(country))
base_data <- base_data %>% 
  left_join(vdem_judconst, by = c("ccode", "year")) %>%
  filter(!(ccode == "692" & year < 2002)) # Current government of Bahrain established in 2002.
rm(vdem_judconst)

# 5. Political corruption (v2x_corr). 
vdem_polcorr <- vdem %>%
  subset(select = c(country, ccode, year, pol_corr)) %>%
  filter(!(ccode == "860" & year < 2001)) %>% 
  filter(!(ccode == "692" & year < 2002)) 
# We need to decide what to do with Bahrain 2002-2004... 

rm(vdem_polcorr)

# 6. Polarization of society (v2smpolsoc). 
vdem_polpolar <- vdem %>%
  subset(select = c(country, ccode, year, pol_polar)) 
# No South Yemen (1950-90), Republic of Vietnam (1950-75), Afghanistan (1950-1991), Papua New Guinea (2021-23), Comoros (2023), Iceland (1950-1991)

rm(vdem_polpolar)

# 7. Civil liberties (v2x_civlib).
vdem_civlib <- vdem %>% 
  subset(select = c(country, ccode, year, civ_lib)) %>% 
  subset(select = -c(country))
base_data <- base_data %>% 
  left_join(vdem_civlib, by = c("ccode", "year"))  
rm(vdem_civlib)

# 8. Women political participation index (v2x_genpp). 
df <- vdem %>%
  subset(select = c(country, ccode, year, wom_polpart))
  



# Women political empowerment index (v2x_gender). 
# Women civil liberties (v2x_gencl). 


df_na <- df[rowSums(is.na(df)) > 0, ]
# df_na <- df[rowSums(is.na(df)) > 0, ]
# df_dup <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
