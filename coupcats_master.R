

###########################################################################
####READ ME!
#CoupCats - please skim to look for "####CT:..." each time you open this.  
#I'm making notes as I clean things to help you learn some stuff.  
#Once you read the note go ahead and delete it.
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

# Data Collection and Statistics
library(tigerstats) # Common statistical analysis methods
library(vcd) # Visualize categorical data using association plots
#####

base_data <- read_csv("https://www.uky.edu/~clthyn2/base_data.csv") # Reading in base data. 

# 0. Country codes (Thyne 2022). 
url <- "https://www.uky.edu/~clthyn2/replace_ccode_country.xls" # Bringing in ccodes to merge. 
destfile <- "replace_ccode_country.xls"
curl::curl_download(url, destfile)
ccodes <- read_excel(destfile)
rm(url, destfile)

#####CT (02/28/25): EMMA - not sure what below is for. Think you probably wrote it before I 
#updated the replace_ccode_country.xls. Unless I'm  missing something, go ahead and delete
#lines 139-146 below (along with this note).

#ccodes <- ccodes %>% # THESE NEED UPDATED! 
#  filter(year == 2022) %>%
#  mutate(year = 2023) %>%
#  bind_rows(ccodes, .) 
#ccodes <- ccodes %>%
#  filter(year == 2022) %>%
#  mutate(year = 2024) %>% 
#  bind_rows(ccodes, .)

# -------------------------- Social Data ------------------------------ #
# 1. Coup data (Powell & Thyne 2011). 
# 1.1. Reading in data. 
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) 

# 1.2. Cleaning up data. 
coup_data <- coup_data %>% # I am getting rid of successes & fails--only if a coup was attempted! 
  select(-ccode_gw, -ccode_polity, -day, -version) %>% 
  mutate(coup_attempted = 1) %>% 
  select(-coup) 
check <- coup_data %>%
  arrange(ccode, year, month) %>%
  mutate(check=ifelse(year==lag(year) & ccode==lag(ccode) & month==lag(month), 1, 0))
  #above telling me we have 7 instances where we had 2+ coups in the same ccode/year/month; just fine to collapse these...
    rm(check)
coup_data <- coup_data %>% 
  distinct(ccode, year, month, .keep_all = TRUE) 

####CT: EMMA - you had "country in the left_join line"; don't do that; just merge by ccode/year/month; 
#those countries can screw you up when USA~=U.S.A., for example. Best just to drop country from the
#DF you're merging into the base_data.

# 1.3. Merging into data set. 
coup_data <- coup_data %>%
  select(-country)
base_data <- base_data %>% 
  left_join(coup_data, by = c("year", "ccode", "month")) %>%
  mutate(coup_attempted = ifelse(is.na(coup_attempted), 0, as.numeric(coup_attempted))) # No NAs. 
rm(coup_data) # Keeping things clean! 



###########################################################################
####CT: reviewed everything above and it's good. Updated 02/28/25.
###########################################################################



# 2. Population data (World Bank Data Group 2024). 
# 2.1. Reading in data. 
url <- "https://extdataportal.worldbank.org/content/dam/sites/data/gender-data/data/data-gen/zip/indicator/population-number.zip"
zip_file <- "population-data.zip"
download.file(url, zip_file, mode = "wb")
unzip(zip_file, exdir = "unzipped_data") 

world_bank <- read.csv("unzipped_data/Population (number).csv") # WB is missing data for 2024. This will update automatically after they add it. 
rm(url, zip_file)

# 2.2. Cleaning up data. 
popln_data <- world_bank %>%
  filter(Indicator.Name == 'Population, total') %>%
  subset(select = c(Country.Name, Year, Value)) %>%
  rename(country = Country.Name,
         year = Year,
         popln_tot = Value) %>% 
  left_join(ccodes, by = c("year", "country")) %>%
  drop_na() # NAs from non-country categories. 

# 2.3. Merging into data set. 
emma_data <- emma_data %>% # No 2024 data--resulting in a lot of missing values. 
  filter(year >= 1960) %>%
  left_join(popln_data, by = c("country", "year", "ccode"), relationship = "many-to-many") %>%
  filter(!(country == "St. Vincent and the Grenadines" & duplicated(paste(year, ccode)))) # For some reason, St. Vincent got weird. 
label(emma_data$popln_tot) <- "total population" 
rm(popln_data)

# 3. Age population data (World Bank Data Group 2024).
# 3.1. Reading in data. 
age_popln <- world_bank %>%
  
  # 3.2. Cleaning up data. 
  filter(Indicator.Name %in% c('Population ages 0-14, total',
                               'Population ages 15-64, total',
                               'Population ages 65 and above, total')) %>%
  subset(select = c(Country.Name, Year, Value, Disaggregation)) %>%
  rename(country = Country.Name,
         year = Year,
         age_tot = Value,
         type = Disaggregation)
age_popln <- age_popln %>%
  left_join(ccodes, by = c("country", "year"), relationship = "many-to-many") %>% 
  drop_na() %>% # NAs from non-country rows. 
  distinct() # Random duplicates. 
age_popln <- age_popln %>%
  pivot_wider(names_from = type, values_from = age_tot, values_fn = first) %>%
  rename(age0_14 = `total, 0-14`, 
         age15_64 = `total, 15-64`,
         age65plus = `total, 65+`) 

# 3.3. Merging into data set. 
emma_data <- emma_data %>% # No 2024 data. 
  left_join(age_popln, by = c("country", "year", "ccode"), relationship = "many-to-many") %>%
  distinct() # Random duplicates. 
rm(world_bank, age_popln)

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
  left_join(ccodes, by = c("country", "year")) %>%
  drop_na() %>% # NAs from non-country rows. 
  distinct() # Random duplicates. 

# 4.3. Merging into data set. 
emma_data <- emma_data %>% 
  left_join(median_age, by = c("country", "year", "ccode"), relationship = "many-to-many") 
rm(median_age)

# 5. Age expectancy data. 
# 5.1. Reading in data. 
url <- "https://srhdpeuwpubsa.blob.core.windows.net/whdh/DATADOT/INDICATOR/C64284D_ALL_LATEST.csv"
age_expectancy <- read_csv(url)
rm(url)

# 5.2. Cleaning up data. 
age_expectancy <- age_expectancy %>%
  subset(select = c(DIM_TIME, GEO_NAME_SHORT, DIM_SEX, AMOUNT_N)) %>%
  filter(DIM_SEX == 'TOTAL') %>%
  rename(country = GEO_NAME_SHORT, 
         year = DIM_TIME, 
         age_expectancy = AMOUNT_N) %>%
  subset(select = -c(DIM_SEX))
age_expectancy <- age_expectancy %>%
  left_join(ccodes, by = c("country", "year")) %>% # Missing 'Côte d'Ivoire', 'Türkiye', 'Zambia', 'Zimbabwe'
  drop_na() %>% # NAs from non-country rows. 
  distinct() # Random duplicates from Yemen. 

# 5.3. Merging into data set. 
emma_data <- emma_data %>% 
  left_join(age_expectancy, by = c("country", "year", "ccode"), relationship = "many-to-many")
rm(age_expectancy)

# ------------------------------------ Military data  ------------------------------------ #

# 1.Military expenditure (% of GDP)
# 1.1 Loading data
url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.XPND.GD.ZS?downloadformat=excel"
destfile <- "MS_MIL_XPND_GD.xls"
curl::curl_download(url, destfile)
military_exp <- read_excel(destfile, skip = 3)
rm(destfile, url)

#1.2 Reshaping data
military_exp <- military_exp %>%
  rename( "country" = `Country Name`) %>% 
  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`))  #removing things we don't want
  
military_exp <- military_exp %>%
  pivot_longer(
    cols = -c(country),  # Keep country-related columns fixed
    names_to = "year",  
    values_to = "military expenditure"
  ) %>%
  mutate(year = as.integer(year))  # Convert Year to integer
label(military_exp$`military expenditure`) <- "(% of GDP)"


# 2.Armed forces personnel (total)
# 2.1 Loading data
url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.TOTL.P1?downloadformat=excel"
destfile <- "MS_MIL_TOTL.xls"
curl::curl_download(url, destfile)
personnel <- read_excel(destfile, skip = 3)
rm(destfile, url)

# 2.2 Reshaping data
personnel <- personnel %>%
  rename( "country" = `Country Name`) %>% 
  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`)) #removing things we don't want

personnel <- personnel %>%
  pivot_longer(
    cols = -c(country),  # Keep country-related columns fixed
    names_to = "year",  
    values_to = "personnel"
  ) %>%
  mutate(year = as.integer(year))  # Convert Year to integer
label(personnel$personnel) <- "(total)"


# 3 Merging
military_data <- military_exp %>%
  left_join(personnel, by = c("country", "year")) %>% 
  left_join(ccodes, by = c("country", "year")) 
rm(military_exp, personnel) #keeping things clean

# Checking NAs: ----- Missing Zimbabwe, Zambia, and "Turkiye" in ccodes
check_na <- military_data %>% 
  filter(is.na(ccode))
unique(check_na$country)
rm(check_na)
