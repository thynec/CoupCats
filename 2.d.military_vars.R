#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building military-related variables

#1. clear all
rm(list = ls())
#2. set working directory
#setwd("~/R/coupcats") # Set working file. 
#setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/teaching/1.coupcast/TEK_S26/git_2026.03.13") #clay at work
#3. install packages
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#------------------------------------------------------------------------------------------------#
#Milex from SIPRI
#------------------------------------------------------------------------------------------------#  

url <- "https://www.sipri.org/sites/default/files/SIPRI-Milex-data-1949-2024_2.xlsx"
download.file(url, destfile = "~/SIPRI-Milex-data-1949-2024.xlsx", mode = "wb")
sipri <- read_excel("~/SIPRI-Milex-data-1949-2024.xlsx", sheet = "Constant (2023) US$", skip=5)
rm(url)

#clean; turn
sipri <- sipri %>%
  rename(country=Country) %>%
  select(-...2, -Notes) %>%
  pivot_longer(
    cols=-c(country),
    names_to="year",
    values_to="milex"
  ) %>%
  mutate(year = as.integer(year)) %>%
  arrange(country, year) %>%
  mutate(milex=as.numeric(milex)) %>%
  arrange(country, year) %>%
  mutate(month=12) %>%
  rename(sipri_milex=milex) %>%
  mutate(year=year+1) #just lagged

sipri <- sipri %>%
  left_join(ccodes, by=c("country", "year"))
#need Congo, DR=490; Congo, Rep=484; Yemen, North=678
sipri <- sipri %>%
  mutate(ccode=ifelse(country=="Congo, DR", 490, ccode)) %>%
  mutate(ccode=ifelse(country=="Congo, Republic", 484, ccode)) %>%
  mutate(ccode=ifelse(country=="Yemen, North", 678, ccode)) %>%
  rename(sipri_country=country) 
sipri <- sipri %>%
  filter(!is.na(sipri_country)) %>%
  filter(!is.na(ccode)) %>%
  arrange(ccode, year) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, NA)) %>% #We're good
  select(-problem, -sipri_country)
check <- sipri %>%
  arrange(ccode, year) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, 0)) #345 and 365 an issue with duplicate years
sipri <- sipri %>%
  filter(!is.na(sipri_milex)) 
rm(check)
base_data <- base_data %>%
  left_join(sipri, by=c("ccode", "year", "month"))
rm(sipri)

#------------------------------------------------------------------------------------------------#
#Milex and milper from COW
#------------------------------------------------------------------------------------------------#  

#Bring in the NMC v6 data; available: https://correlatesofwar.org/data-sets/national-material-capabilities/
url <- "https://correlatesofwar.org/wp-content/uploads/NMC_Documentation-6.0.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
unzip("data/NMC-60-wsupplementary.zip", exdir="NMC_data")
nmc <- read_dta("NMC_data/NMC-60-wsupplementary.dta")
unlink("data", recursive=TRUE)
unlink("NMC_data", recursive=TRUE)
rm(url)
nmc <- nmc %>%
  select(statenme, ccode, year, milex, milper) %>%
  rename(cow_milex=milex) %>%
  rename(cow_milper=milper) %>%
  rename(cow_country=statenme) %>%
  mutate(year=year+1) %>% #just lagged
  mutate(cow_milex=ifelse(cow_milex<0, NA, cow_milex)) %>%
  mutate(cow_milper=ifelse(cow_milper<0, NA, cow_milper)) %>%
  mutate(month=12)
#feels like linear interpolation (but not extrapolation) should be fine for milex
#feels like linear interpolation (but not extrapolation) should be fine for milper
#merge into base
base_data <- base_data %>%
  left_join(nmc, by=c("ccode", "year", "month"))
rm(nmc)
cor.test(base_data$sipri_milex, base_data$cow_milex) #r=.867 so matching up about what we'd expect
check <- base_data %>%
  filter(country!=cow_country) %>%
  select(country, cow_country) %>%
  distinct() #we're good
rm(check) 
base_data <- base_data %>%
  select(-cow_country)

#------------------------------------------------------------------------------------------------#
#Splice SIPRI/COW milex
#------------------------------------------------------------------------------------------------#  

df <- base_data %>%
  select(country, ccode, year, month, sipri_milex, cow_milex) %>%
  filter(month==12) %>%
  distinct() %>%
  select(-month)
df <- df %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(ch=((sipri_milex-lag(sipri_milex))/lag(sipri_milex)))
df <- df %>%
  group_by(ccode) %>%
  mutate(splice=cow_milex) %>%
  mutate(ch=ifelse(ccode==101 & year==2018, 1, ch)) %>% #Ven 1018 weird because went from 0 to 1, so ch divided by 0; fixed it with this
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))
df <- df %>%
  select(ccode, year, splice) %>%
  rename(milex_spliced=splice) %>%
  mutate(milex_spliced=ifelse(is.nan(milex_spliced), 0, milex_spliced)) %>%
  mutate(month=12)
base_data <- base_data %>%
  ungroup() %>%
  left_join(df, by=c("ccode", "year", "month")) %>%
  select(-sipri_milex,  -cow_milex)
rm(df)

#create linear interpolation/extrapolation
base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  mutate(milex_spliced_OG=milex_spliced) %>%
  group_by(ccode) %>%
  mutate(
    time_id = year + (month - 1)/12,
    milex_spliced = na.approx(
      milex_spliced,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  ungroup() %>%
  select(-time_id)
#log values for milex
base_data <- base_data %>%
  mutate(milex_spliced=log(milex_spliced+1)) %>%
  set_variable_labels(milex_spliced="SIPRI+COW, t-1, log, linear interpolation")

#------------------------------------------------------------------------------------------------#
#Milper from WDI
#------------------------------------------------------------------------------------------------#  

wdi <- WDI(country = "all", indicator = "MS.MIL.TOTL.P1", start = 1960, end = 2026)
wdi <- wdi %>%
  select(country, year, MS.MIL.TOTL.P1) %>%
  rename(wdi_milper = MS.MIL.TOTL.P1) %>%
  mutate(year=year+1) %>% #just lagged
  left_join(ccodes, by=c("country", "year"))
check <- wdi %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct()
rm(check)
wdi <- wdi %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  mutate(ccode=ifelse(country=="Somalia, Fed. Rep.", 520, ccode)) %>%
  rename(wdi_country=country) %>%
  filter(!is.na(ccode)) %>%
  select(-wdi_country) %>%
  mutate(month=12)
base_data <- base_data %>%
  left_join(wdi, by=c("ccode", "year", "month"))
rm(wdi)

#bounce it back to country year, deal with missing data in wdi, then splice
df <- base_data %>%
  select(ccode, year, month, cow_milper, wdi_milper) %>%
  filter(month==12) %>%
  distinct()
df <- df %>%
  group_by(ccode) %>%
  mutate(ch=(wdi_milper-lag(wdi_milper))/lag(wdi_milper))
df <- df %>%
  group_by(ccode) %>%
  mutate(splice=cow_milper) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  ungroup()
df <- df %>%
  select(ccode, year, splice) %>%
  rename(milper_spliced=splice) %>%
  mutate(month=12)
base_data <- base_data %>%
  ungroup() %>%
  left_join(df, by=c("ccode", "year", "month")) %>%
  select(-cow_milper, -wdi_milper)
rm(df)

base_data <- base_data %>%
  mutate(milper_spliced_OG=milper_spliced) %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    time_id = year + (month - 1)/12,
    milper_spliced = na.approx(
      milper_spliced,
      x = time_id,
      na.rm = FALSE,
      rule = 2
    )
  ) %>%
  ungroup() %>%
  select(-time_id)
base_data <- base_data %>%
  mutate(solqual=log(milex_spliced/(milper_spliced+1))+1)

#------------------------------------------------------------------------------------------------#  
#mutiny data from Powell and Johnson
#------------------------------------------------------------------------------------------------#  

#johnson; 1945-2017
temp <- tempfile(fileext = ".xlsx")
download.file("https://jaclynjohnson.weebly.com/uploads/2/6/4/7/26479187/mmdd.xlsx", 
              destfile = temp, mode = "wb")
mutiny_johnson <- read_excel(temp)
rm(temp)
mutiny_johnson <- mutiny_johnson %>%
  filter(region != "4") #remove African countries from this dataset to bring in more accurate data
mutiny_johnson <- mutiny_johnson %>%
  arrange(`Event date`)
mutiny_johnson <- mutiny_johnson %>%
  select('Country', 'ccode', 'month', 'day', 'year')

#powell; 1954-2023
mutiny_africa <- read.csv("https://docs.google.com/spreadsheets/d/1HYbOOfWSMDfhyz3jGAq9Bg8pNeH-z6aB/export?format=csv&gid=963832040")
mutiny_africa <- mutiny_africa %>%
  select('cname', 'ccode', 'year', 'month', 'edate')
mutiny_africa$edate <- day(mdy(mutiny_africa$edate))
mutiny_africa <- mutiny_africa %>%
  rename(Country = cname) %>%
  rename(day = edate)
mutiny_africa <- mutiny_africa %>%
  mutate(month=ifelse(month==21, 12, month)) %>%
  mutate(day=ifelse(is.na(day), 15, day))

#combine
mutiny <- bind_rows(mutiny_johnson, mutiny_africa)
rm(mutiny_africa, mutiny_johnson)

df <- mutiny %>%
  mutate(
    date = ymd(paste(year, month, day)),
    year = year(date),
    month = month(date),
    mutiny = 1
  ) %>%
  select(ccode, year, month, mutiny) %>%
  distinct()

base_data <- base_data %>%
  left_join(df, by=c("ccode", "year", "month"))
rm(df)

base_data <- base_data %>%
  arrange(ccode, year, month) %>%
  group_by(ccode) %>%
  mutate(
    mutiny6 = rollapplyr(
      mutiny,
      width = 6,
      FUN = function(x) as.integer(any(x == 1)),
      fill = 0
    ),
    mutiny12 = rollapplyr(
      mutiny,
      width = 12,
      FUN = function(x) as.integer(any(x == 1)),
      fill = 0
    )
  ) %>%
  ungroup()
rm(mutiny)

base_data <- base_data %>%
  mutate(mutiny6=ifelse(is.na(mutiny6) & year<=2018, 0, mutiny6)) %>%
  mutate(mutiny12=ifelse(is.na(mutiny12) & year<=2018, 0, mutiny12)) %>%
  select(-mutiny) %>%
  set_variable_labels(mutiny6="1 if mutiny in last 6 months") %>%
  set_variable_labels(mutiny12="1 if mutiny in last 12 months")

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.d.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  











#------------------------------------------------------------------------------------------------#
#below is stuff we probably won't use
#note that we want total milex in constant dollars, which WDI no longer provides; that's why I'm using
#SIPRI plus COW above
#------------------------------------------------------------------------------------------------#    

#
#
#
#
#
#
## Interpolate using dplyr + approx()
#df <- df %>%
#  mutate(y_interp = approx(x, y, xout = x, method = "linear", rule = 2)$y)
#
#
#
#
#
#
#
#
#
#
#
#
#wdi_data <- WDI(indicator = c("MS.MIL.XPND.KD"), country="all", start = 1960, end = 2023)
#
#
#
#wdi_data <- WDI(indicator = c("MS.MIL.XPND.KD"), country = "all", start = 1996, end = 2023, extra = TRUE) # Data are reported every two years--need to figure out a way to transform.   
#
#
#
#
#
#
#
#
#
##Loading data
#url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.XPND.CD.?downloadformat=excel"
#destfile <- "MS_MIL_XPND_CD.xls"
#curl::curl_download(url, destfile)
#milex <- read_excel(destfile, skip = 3)
#rm(destfile, url)worl
#
##Reshaping data
#milex <- milex %>%
#  rename( "country" = `Country Name`) %>% 
#  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`))  #removing things we don't want
#milex <- milex %>%
#  pivot_longer(
#    cols = -c(country),  # Keep country-related columns fixed
#    names_to = "year",  
#    values_to = "military expenditure"
#  ) %>%
#  mutate(year = as.integer(year))  # Convert Year to integer
#
##merge to base
#milex <- milex %>%
#  rename(milex='military expenditure') %>%
#  set_variable_labels(milex="Milit expenditure, % of GDP") %>%
#  mutate(year=year+1) #lagged
#milex <- milex %>%
#  left_join(ccodes, by=c("country", "year"))
##need to fix turkey=640
#milex <- milex %>%
#  mutate(ccode=ifelse(country=="Turkiye", 640, ccode))
#milex <- milex %>%
#  rename(wdi_country=country)
#base_data <- base_data %>%
#  left_join(milex, by=c("ccode", "year"))
#rm(milex)
#
##------------------------------------------------------------------------------------------------#
##milper from WB
##------------------------------------------------------------------------------------------------#  
#
##Loading data
#url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.TOTL.P1?downloadformat=excel"
#destfile <- "MS_MIL_TOTL.xls"
#curl::curl_download(url, destfile)
#milper <- read_excel(destfile, skip = 3)
#rm(destfile, url)
#
##Reshaping data; cleaning a bit
#milper <- milper %>%
#  rename( "country" = `Country Name`) %>% 
#  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`)) #removing things we don't want
#milper <- milper %>%
#  pivot_longer(
#    cols = -c(country),  # Keep country-related columns fixed
#    names_to = "year",  
#    values_to = "milper"
#  ) %>%
#  mutate(year = as.integer(year))  # Convert Year to integer
#
##merge to base
#milper <- milper %>%
#  rename(milper='military personell') %>%
#  set_variable_labels(milper="Milit expenditure, % of GDP") %>%
#  mutate(year=year+1) #lagged
#milex <- milex %>%
#  left_join(ccodes, by=c("country", "year"))
##need to fix turkey=640
#milex <- milex %>%
#  mutate(ccode=ifelse(country=="Turkiye", 640, ccode))
#milex <- milex %>%
#  rename(wdi_country=country)
#base_data <- base_data %>%
#  left_join(milex, by=c("ccode", "year"))
#rm(milex)
#
#
#
#
##------------------------------------------------------------------------------------------------#  
##milex, milper info from peacesciencer
##------------------------------------------------------------------------------------------------#  
##
