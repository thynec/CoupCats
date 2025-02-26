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
  select(-coup) %>% 
  distinct(ccode, year, month, .keep_all = TRUE) 

# 1.3. Merging into data set. 
emma_data <- base_data %>% 
  left_join(coup_data, by = c("year", "country", "ccode", "month")) %>%
  mutate(coup_attempted = ifelse(is.na(coup_attempted), 0, as.numeric(coup_attempted))) # No NAs. 
label(emma_data$coup_attempted) <- "2 = successful, 1 = failed"
rm(base_data, coup_data) # Keeping things clean! 

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
