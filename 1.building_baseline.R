hello world!

# -------------------------- Baseline Data ------------------------------ #

rm(list=ls()) 

base_data <- read_csv("https://www.uky.edu/~clthyn2/base_data.csv") # Reading in base data. 

# Country codes (Thyne 2022). 
url <- "https://www.uky.edu/~clthyn2/replace_ccode_country.xls" # Bringing in ccodes to merge. 
destfile <- "replace_ccode_country.xls"
curl::curl_download(url, destfile)
ccodes <- read_excel(destfile)
rm(url, destfile)

#update so we have most recent month: 04/2025
df <- base_data %>%
  group_by(country) %>%
  filter(year==2024) %>%
  mutate(year=year+1) %>%
  filter(month<5) %>%
  ungroup()
base_data <- base_data %>%
  full_join(df, by=c("ccode", "year", "month", "country"))
rm(df)

# Coup data (Powell & Thyne 2011). 
# Reading in data. 
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) 

# Cleaning up data. 
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

# Merging into data set. 
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

# Set up non-coup months to deal with autocorrelation in the analyses
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

