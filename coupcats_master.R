###########################################################################
####READ ME!
#CoupCats - please skim to look for "####CT:..." each time you open this.  I'm making notes as I clean things to help you learn some stuff.  Once you read the note go ahead and delete it.
###########################################################################

rm(list=ls())

#setwd("C:/Users/Camila/OneDrive/R related/R/R") # Set working file. #Camila
setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home

#load packages: if needed, go in and remove # but don't forget to put it back#
#####
# Data Management
#install.packages("tidyverse") # Primary package for data management
#install.packages("readr") # Reads flat files (i.e., CSV)
#install.packages("openxlsx") # Reads (and writes) Excel files
#install.packages("haven") # Imports & exports SPSS, SAS, and Stata files
#install.packages("janitor") # Cleans column names and tidies data
#install.packages("Hmisc") # For labeling data. 
#install.packages("pltesim") #For setting up bctcs peace year

# Data Visualization
#install.packages("ggplot2") # Primary package for data visualization
#install.packages("gganimate") # Creates animated plots (with ggplot2)
#install.packages("ggfortify") # Visualizes model outputs (PCA, clustering)
#install.packages("ggmosaic") # Creates mosaic plots
#install.packages("ggpubr") # Enhances ggplot2 with greater details
#install.packages("ggstance") # Horizontal geoms for ggplot2
#install.packages("plotly") # Create interactive plots

# Statistical Analysis and Modeling
#install.packages("broom") # Tidy model outputs into data frames
#install.packages("car") # Regression diagnostics and statistical methods
#install.packages("DescTools") # Descriptive stats and hypothesis testing
#install.packages("emmeans") # Estimated marginal means and model comparisons
#install.packages("olsrr") # Linear regression diagnostics and model selection
#install.packages("lme4") # Linear and generalized linear mixed-effects models
#install.packages("MASS") # Various statistical models and methods

# Spatial Analysis
#install.packages("sf") # Handle spatial data and analysis
#install.packages("spData") # Spatial datasets for use with sf
#install.packages("terra") # Work with raster data and spatial analysis

# Report Generation
#install.packages("rmarkdown") # Dynamic report generation
#install.packages("knitr") # Process and display R code/output in reports

# Model Diagnostics and Evaluation
#install.packages("performance") # Evaluate model performance and diagnostics
#install.packages("DHARMa") # Residual diagnostics for GLMs

# Data Summarization and Reporting
#install.packages("gtsummary") # Summarizes regression models
#install.packages("summarytools") # Quick summaries of your data
#install.packages("stargazer") # Creates regression tables

# Data Export and Import
#install.packages("remotes") # #install R packages from GitHub or other sources

# Miscellaneous Utilities
#install.packages("gapminder") # Global development indicators and example data
#install.packages("peacesciencer")  # Political science/peace studies tools
#install.packages("rcompanion") # Companion functions for statistical tasks
#install.packages("rgl") # 3D visualization
#install.packages("WDI") # World Bank Worldwide Data Indicators 

# Data Collection and Statistics
#install.packages("tigerstats") # Common statistical analysis methods
#install.packages("vcd") # Visualize categorical data using association plots
#####
#load libraries
#####
# Data Management
library(tidyverse) # Primary package for data management
library(readr) # Reads flat files (i.e., CSV)
library(openxlsx) # Reads (and writes) Excel files
library(haven) # Imports & exports SPSS, SAS, and Stata files
library(janitor) # Cleans column names and tidies data
library(Hmisc) # For labeling data. 
library(readxl) # To read excel sheets.
library(pltesim) # To set up btscs; see https://www.rdocumentation.org/packages/DAMisc/versions/1.7.2/topics/btscs

# Data Visualization
library(ggplot2) # Primary package for data visualization
library(gganimate) # Creates animated plots (with ggplot2)
library(ggfortify) # Visualizes model outputs (PCA, clustering)
library(ggmosaic) # Creates mosaic plots
library(ggpubr) # Enhances ggplot2 with greater details
library(ggstance) # Horizontal geoms for ggplot2
library(plotly) # Create interactive plots

# Statistical Analysis and Modeling
library(broom) # Tidy model outputs into data frames
library(car) # Regression diagnostics and statistical methods
library(DescTools) # Descriptive stats and hypothesis testing
library(emmeans) # Estimated marginal means and model comparisons
library(olsrr) # Linear regression diagnostics and model selection
library(lme4) # Linear and generalized linear mixed-effects models
library(MASS) # Various statistical models and methods

# Spatial Analysis
library(sf) # Handle spatial data and analysis
library(spData) # Spatial datasets for use with sf
library(terra) # Work with raster data and spatial analysis

# Report Generation
library(rmarkdown) # Dynamic report generation
library(knitr) # Process and display R code/output in reports

# Model Diagnostics and Evaluation
library(performance) # Evaluate model performance and diagnostics
library(DHARMa) # Residual diagnostics for GLMs

# Data Summarization and Reporting
library(gtsummary) # Summarizes regression models
library(summarytools) # Quick summaries of your data
library(stargazer) # Creates regression tables

# Data Export and Import
library(remotes) # Install R packages from GitHub or other sources

# Miscellaneous Utilities
library(gapminder) # Global development indicators and example data
library(peacesciencer)  # Political science/peace studies tools
library(rcompanion) # Companion functions for statistical tasks
library(rgl) # 3D visualization
library(WDI) # World Bank Worldwide Data Indicators 

# Data Collection and Statistics
library(tigerstats) # Common statistical analysis methods
library(vcd) # Visualize categorical data using association plots
#####

# -------------------------- Baseline Data ------------------------------ #

base_data <- read_csv("https://www.uky.edu/~clthyn2/base_data.csv") # Reading in base data. 

# 0. Country codes (Thyne 2022). 
url <- "https://www.uky.edu/~clthyn2/replace_ccode_country.xls" # Bringing in ccodes to merge. 
destfile <- "replace_ccode_country.xls"
curl::curl_download(url, destfile)
ccodes <- read_excel(destfile)
rm(url, destfile)

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
