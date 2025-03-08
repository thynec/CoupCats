#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building domestic economic variables
#Priority vars:
  #wealth
  #change in wealth over time
  #economic inequality
#Secondary vars:
  #inflation (probably CPI)
  #resources (see Powell/Schiel/Hammou J. Global Sec Studies, 2021; Lango/Bell/Wolford Intl Interactions 2022)

#1. clear all
  rm(list = ls())
#2. set working directory
  #setwd("~/R/coupcats") # Set working file. 
  #setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
#3. install packages
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
  source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#------------------------------------------------------------------------------------------------#
#put all new/revised coding below
#------------------------------------------------------------------------------------------------#  