#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building military-related variables

#Priority vars:
  #military quality (milex/milper)

#Secondary vars:
  #counterbalancing
  #civil-military data from Narang/Talmadge 2017
  #purges from Sudduth
  #military integration into politics (Croissan/Eschenauer/Kamerling Euro Pol Science)
  #state security forces (De Bruin J. of Peace Research 2020)
  #pro-government militias

#1. clear all
  rm(list = ls())
#2. set working directory
  #setwd("~/R/coupcats") # Set working file. 
  #setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
  #setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/current_research/coupcats") #clay at work
#3. install packages
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
  source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#------------------------------------------------------------------------------------------------#
#put all new/revised coding below
#------------------------------------------------------------------------------------------------#  

#------------------------------------------------------------------------------------------------#  
#milex, milper info from peacesciencer
#------------------------------------------------------------------------------------------------#  







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
#clean up if needed and export
write.csv(base_data, gzfile("2.d.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  



