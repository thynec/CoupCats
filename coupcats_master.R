###########################################################################
####READ ME!
#CoupCats - please skim to look for "####CT:..." each time you open this.  I'm making notes as I clean things to help you learn some stuff.  Once you read the note go ahead and delete it.
###########################################################################

rm(list=ls())

#setwd("C:/Users/Camila/OneDrive/R related/R/R") # Set working file. #Camila
setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home

#load packages: if you use new packages, try to remember to add them to the packages on github. Otherwise, just flag them and I'll do it.
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R")

#load libraries: if you add new libraries, try to remember to add them to github. Otherwise, just flag them and I'll do it.
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R")

# -------------------------- Baseline Data ------------------------------ #

base_data <- read_csv("https://www.uky.edu/~clthyn2/base_data.csv") # Reading in base data. 

# 0. Country codes (Thyne 2022). 
url <- "https://www.uky.edu/~clthyn2/replace_ccode_country.xls" # Bringing in ccodes to merge. 
destfile <- "replace_ccode_country.xls"
curl::curl_download(url, destfile)
ccodes <- read_excel(destfile)
rm(url, destfile)

# -------------------------- Int Signals ----------------------------- #

# 1. WEIS DATA
# 1.1. Getting the data
url <- "https://github.com/thynec/CoupCats/raw/main/weis.fromlai.1966-93.dta" #bringing in the data
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

# 1.3. Aggregating by month    # We figured that it is possibly better to do this after we merge both datasets and have them in the same scale
# weis <- weis %>%
  #group_by(year, month, target) %>%
  #summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") #aggregating by month, year, and target

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

# -------------------------- V-Dem Data ------------------------------ #

# 1. V-Dem data. 
# 1.1. Reading in data. 
vdem <- vdem # Code looks strange, but it works. 

'The V-Dem dataset does not cover some countries, namely: 
  Andorra, Antigua and Barbuda, Bahamas, Belize, Brunei, Dominica, 
  Federated States of Micronesia, Grenada, Kiribati, Liechtenstein, 
  Marshall Islands, Monaco, Nauru, Palau, Saint Kitts and Nevis, 
  Saint Lucia, Saint Vincent and the Grenadines, Samoa, San Marino, 
  Tonga, Tuvalu, and the Vatican.'

# 1.2. Cleaning up data. 
vdem <- vdem %>%
  subset(select = c(country_name, # Country. 
                    e_regionpol_6C, # Region. 
                    year, # Year. 
                    v2x_regime, # Regime type. 
                    v2x_execorr, # Executive corruption index.
                    v2x_jucon, # Judicial constraints on the executive index ordinal
                    v2x_corr, # Political corruption. 
                    v2smpolsoc, # Polarization of society. 
                    v2cagenmob, # Mass mobilization. 
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
         regime = v2x_regime, # No 1990 for Kazakhstan or Turkmenistan: both gained independence in 1991
         exec_corr = v2x_execorr, # No data between 1950-2000 for Timor-Leste: gained independence in 2000. 
         jud_const = v2x_jucon, # Ditto Timor-Leste; No 1950-2001 for Bahrain, current government established in 2002.
         pol_corr = v2x_corr, # Ditto Timor-Leste & Bahrain. 
         soc_polar = v2smpolsoc, # Between 2000 to 2023 (no other NAs).
         mass_mobil = v2cagenmob, # No Iceland, Papua New Guinea (2021-2023), Afghanistan (1950-1999), Vietnam (1950-1975), South Yemen (1950-1990). 
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
  select(ccode, everything()) %>%
  subset(select = -c(country))

# 1.3. Merging into data set. 
base_data <- base_data %>% 
  left_join(vdem, by = c("ccode", "year")) # No duplicates. 

# -------------------------- Political Data ------------------------------ #

# 1. Perceptions of Corruption (Transparency International, Corruption Perceptions Index). 
# 1.1. Reading in data. 
wdi_data <- WDI(indicator = c("CC.EST", "CC.PER.RNK"), country = "all", start = 1996, end = 2023, extra = TRUE) # Data are reported every two years--need to figure out a way to transform. 

# 1.2. Cleaning up data. 
wdi_data <- wdi_data %>%
  mutate(year=year+1) %>% #just lagged by adding a year
  subset(select = c(country, year, CC.EST, CC.PER.RNK, region)) %>% 
  rename(corruption_est = CC.EST, 
         corruption_rank = CC.PER.RNK,
         wdi_region = region) %>%
  set_variable_labels(
    corruption_est="Control of Corruption: Estimate; from WDI; t-1",
    corruption_rank="Control of Corruption: Percentile Rank; from WDI; t-1",
    wdi_region="regions from WDI"
  )
wdi_data <- wdi_data %>%
  left_join(ccodes, by = c("year", "country")) 
check <- wdi_data %>%
  filter(if_any(everything(), is.na)) 
table(check$country)
table(check$year)
#check looks fine; missing 3 in normal places due to temporal coverage; other missings are due to small countries or regions
rm(check) 

# 1.3. Merging into data set. Do this twice. First for corruption by ccode/year; then for region by ccode (region doesn't vary by time)
wdi1 <- wdi_data %>%
  select(-country, -wdi_region)
base_data <- base_data %>% 
  left_join(wdi1, by = c("year", "ccode")) # NAs are unreported years. 
wdi2 <- wdi_data %>%
  select(ccode, wdi_region) %>%
  distinct()
base_data <- base_data %>%
  left_join(wdi2, by=c("ccode"))
rm(wdi_data, wdi1, wdi2)

#Add regional dummies
table(base_data$wdi_region)
base_data <- base_data %>%
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

# 2. Regime data (V-Dem) 
# 2.1. Reading in data. 
url <- "https://github.com/vdeminstitute/vdemdata/raw/master/data/vdem.RData"
destfile <- tempfile(fileext = ".RData") 
download.file(url, destfile, mode = "wb")
load(destfile)
rm(destfile, url)

# 2.2. Cleaning up data. 
regime_type <- vdem %>%
  subset(select = c(country_name, year, v2x_regime)) %>%
  rename(country = country_name, 
         regime_type = v2x_regime) %>%
  filter(year >= 1950)
regime_type <- regime_type %>% 
  left_join(ccodes, by = c("country", "year")) %>% # NAs resulting from state-like actors, not full states.  
  subset(select = -c(country)) %>% # To prevent future duplicated columns. 
  drop_na() # No duplicates either! 
rm(vdem)
label(regime_type$regime_type) <- "0 = Closed autocracy, 1 = Electoral autocracy, 2 = Electoral democracy, 3 = Liberal Democracy"

# 2.3 Organizing variables for regression 
regime_type <- regime_type %>%
  mutate(
    closed_autocracy = ifelse(regime_type == 0, 1, 0),
    electoral_autocracy = ifelse(regime_type == 1, 1, 0),
    electoral_democracy = ifelse(regime_type == 2, 1, 0),
    liberal_democracy = ifelse(regime_type == 3, 1, 0)
  )

# 2.4. Merging into data set. 
base_data <- base_data %>% 
  left_join(regime_type, by = c("ccode", "year")) # Missing data simply is not updated by V-Dem, so I will not be dropping them. 
rm(regime_type)


# -------------------------- Social Data ------------------------------ #
# 1. Coup data (Powell & Thyne 2011). 
# 1.1. Reading in data. 
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) 

# 1.2. Cleaning up data. 
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

# 1.3. Merging into data set. 
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

# 1.4. Set up non-coup months to deal with autocorrelation in the analyses
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
#For above, peace years are set up ignoring success/failed; go back and re-created these if you end up wanting to analyze success/failed instead of all attempts

# 2. Population data (World Bank Data Group 2024). 
# 2.1. Reading in data. 
url <- "https://extdataportal.worldbank.org/content/dam/sites/data/gender-data/data/data-gen/zip/indicator/population-number.zip"
zip_file <- "population-data.zip"
download.file(url, zip_file, mode = "wb")
unzip(zip_file, exdir = "unzipped_data") 
world_bank <- read.csv("unzipped_data/Population (number).csv") # WB is missing data for 2024. This will update automatically after they add it. 
rm(url, zip_file)
unlink("population-data.zip")
unlink("unzipped_data", recursive=TRUE)

# 2.2. Cleaning up data. 
pop <- world_bank %>%
  filter(Indicator.Name == 'Population, total') %>%
  subset(select = c(Country.Name, Year, Value)) %>%
  rename(country = Country.Name,
         year = Year,
         population = Value) %>% 
  left_join(ccodes, by = c("year", "country")) 
check <- pop %>%
  filter(if_any(everything(), is.na))
table(check$country) #all regions or tiny countries; we're fine to drop NAs
pop <- pop %>%
  drop_na() %>%
  select(-country)
rm(check)
check <- pop %>%
  distinct() #no duplicates in pop data
check <- base_data %>%
  distinct() #no duplicates in base data
rm(check)
pop <- pop %>%
  mutate(year=year+1)  %>%
  mutate(pop=log(population)) %>%
  set_variable_labels(
    pop="Total pop, WDI, log, t-1"
  ) %>%
  select(-population)

# 2.3. Merging into data set. 
base_data <- base_data %>% 
  ungroup() %>%
  left_join(pop, by = c("ccode", "year"))
rm(pop)
check <- base_data %>%
  filter(is.na(pop))
histogram(check$year) #looks fine but should be able to get pop for full sample from elsewhere
rm(check)
rm(world_bank)

# 4. Median age data (World Bank Data Group 2024)
# 4.1. Reading in data. 
url <- "https://ourworldindata.org/grapher/median-age.csv?v=1&csvType=full&useColumnShortNames=true"
median_age <- read_csv(url)
rm(url)

# 4.2. Cleaning up data. 
median_age <- median_age %>%
  subset(select = c(Entity, Year, median_age__sex_all__age_all__variant_estimates, median_age__sex_all__age_all__variant_medium)) %>%
  rename(country = Entity,
         year = Year,
         popln_median = median_age__sex_all__age_all__variant_estimates,
         popln_median_est = median_age__sex_all__age_all__variant_medium) %>%
  mutate(median_age = ifelse(is.na(popln_median), popln_median_est, popln_median)) %>%
  subset(select = -c(popln_median, popln_median_est)) %>% 
  filter(year <= 2024)
median_age <- median_age %>%
  mutate(year=year+1) %>% #just lagged
  left_join(ccodes, by = c("country", "year")) 
check <- median_age %>%
  filter(is.na(ccode))
table(check$country) #fine; all regions or very small countries
rm(check)
median_age <- median_age %>%
  drop_na() %>% # NAs from non-country rows. 
  distinct() %>% # Random duplicates. 
  select(-country)

# 4.3. Merging into data set. 
base_data <- base_data %>% 
  left_join(median_age, by = c("ccode", "year")) 
rm(median_age)

# ------------------------------------ Military data  ------------------------------------ #

# 1.Military expenditure (% of GDP)
# 1.1 Loading data
url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.XPND.GD.ZS?downloadformat=excel"
destfile <- "MS_MIL_XPND_GD.xls"
curl::curl_download(url, destfile)
milex <- read_excel(destfile, skip = 3)
rm(destfile, url)

#1.2 Reshaping data
milex <- milex %>%
  rename( "country" = `Country Name`) %>% 
  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`))  #removing things we don't want
milex <- milex %>%
  pivot_longer(
    cols = -c(country),  # Keep country-related columns fixed
    names_to = "year",  
    values_to = "military expenditure"
  ) %>%
  mutate(year = as.integer(year))  # Convert Year to integer

#merge to base
milex <- milex %>%
  rename(milex='military expenditure') %>%
  set_variable_labels(milex="Milit expenditure, % of GDP") %>%
  mutate(year=year+1) #lagged
milex <- milex %>%
  left_join(ccodes, by=c("country", "year")) %>%
  select(-country) %>%
  distinct()
base_data <- base_data %>%
  left_join(milex, by=c("ccode", "year"))
rm(milex)

# 2.Armed forces personnel (total)
# 2.1 Loading data
url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.TOTL.P1?downloadformat=excel"
destfile <- "MS_MIL_TOTL.xls"
curl::curl_download(url, destfile)
milper <- read_excel(destfile, skip = 3)
rm(destfile, url)

# 2.2 Reshaping data; cleaning a bit
milper <- milper %>%
  rename( "country" = `Country Name`) %>% 
  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`)) #removing things we don't want
milper <- milper %>%
  pivot_longer(
    cols = -c(country),  # Keep country-related columns fixed
    names_to = "year",  
    values_to = "milper"
  ) %>%
  mutate(year = as.integer(year))  # Convert Year to integer
milper <- milper %>%
  drop_na() %>%
  mutate(milper=log(milper+1)) %>% #just logged
  mutate(year=year+1) %>% #just lagged
  set_variable_labels(milper="milit personell; logged; t-1")

# 3 Merging
milper <- milper %>%
  left_join(ccodes, by=c("country", "year")) %>%
  drop_na() %>%
  select(-country)
base_data <- base_data %>%
  left_join(milper, by=c("ccode", "year"))
rm(milper)

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
write.csv(base_data, "base_data.csv", row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################

#### Filling in NAs below, committed on 3/7 ####
       
# Source the interpolation functions script
source("interpolation_functions.R")

# Load the dataset
url <- "https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/base_data.csv"
df <- read.csv(url)

# Check for missing values
values <- check_missing_values(df)

# Fill missing values using forward/backward fill
df <- fill_missing_values(df, values)

# Interpolate missing values
for (val in values) {
  df <- interpolate_missing_values(df, val, max_gap = 36, method = 'spline', order = 3)
}

# Check remaining missing values
cat("\nFinal missing values check:\n")
for (col in c(values, paste0(values, '_interpolated'))) {
  if (col %in% colnames(df)) {
    na_count <- sum(is.na(df[[col]]))
    if (na_count > 0) {
      total <- nrow(df)
      pct <- na_count / total * 100
      cat(sprintf("%s has %d / %d rows (%.2f%%) missing values\n", col, na_count, total, pct))
    }
  }
}

#### End of Commit ####







###############################################################################################
#Below is stuff we probably won't use but keeping it here just in case...
###############################################################################################

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
