#Data sources
library(vdemdata) #Vdem
library(WDI) #World Development Indicators; World Bank
library(peacesciencer) #Political science/peace studies tools

# Data Management
library(tidyverse) # Primary package for data management
library(dplyr)
library(readr) # Reads flat files (i.e., CSV)
library(openxlsx) # Reads (and writes) Excel files
library(haven) # Imports & exports SPSS, SAS, and Stata files
library(janitor) # Cleans column names and tidies data
library(Hmisc) # For labeling data. 
library(labelled) # For labeling data.
library(lubridate) # For working with dates
library(readxl) # To read excel sheets.
library(pltesim) # To set up BTSCS / temporal dependence
library(broom) # Tidy model outputs into data frames
library(data.table) # Fast data operations / large datasets
library(splitstackshape) # Expand rows using 'expandRows'
library(countrycode) # Convert country codes (iso, names, etc.)
library(stringr) # String manipulation

# Data Visualization
library(ggplot2) # Primary package for data visualization
library(gganimate) # Creates animated plots (with ggplot2)
library(ggfortify) # Visualizes model outputs (PCA, clustering)
library(ggmosaic) # Creates mosaic plots
library(ggpubr) # Enhances ggplot2 with greater details
library(ggstance) # Horizontal geoms for ggplot2
library(plotly) # Create interactive plots
library(cowplot) # ggplot add on
library(scales) # Color code significance level

# Statistical Analysis and Modeling
library(car) # Regression diagnostics and statistical methods
library(DescTools) # Descriptive stats and hypothesis testing
library(emmeans) # Estimated marginal means and model comparisons
library(olsrr) # Linear regression diagnostics and model selection
library(lme4) # Linear and generalized linear mixed-effects models
library(MASS) # Various statistical models and methods
library(margins) # Calculate marginal effects
library(fixest) # Fixed effects, clustering, AR(1), fast estimation

# Spatial Analysis
library(sf) # Handle spatial data and analysis
library(spData) # Spatial datasets for use with sf
library(terra) # Work with raster data and spatial analysis

# Report Generation
library(rmarkdown) # Dynamic report generation
library(knitr) # Process and display R code/output in reports
library(officer) # Create and edit Word documents
library(flextable) # Format tables for Word/PowerPoint

# Model Diagnostics and Evaluation
library(performance) # Evaluate model performance and diagnostics
library(DHARMa) # Residual diagnostics for GLMs
library(lmtest) # Linear regression diagnostics

# Data Summarization and Reporting
library(gtsummary) # Summarizes regression models
library(summarytools) # Quick summaries of your data
library(stargazer) # Creates regression tables
library(gt) # Used for regression tables

# Data Export and Import
library(remotes) # Install R packages from GitHub or other sources

# Miscellaneous Utilities
library(gapminder) # Global development indicators and example data
library(rcompanion) # Companion functions for statistical tasks
library(rgl) # 3D visualization

# Data Collection and Statistics
library(tigerstats) # Common statistical analysis methods
library(vcd) # Visualize categorical data using association plots

# Data analysis
library(aod)

# Interpolation / Time Series
library(zoo) # Rolling functions, interpolation
library(dplyr) # Data manipulation (lead/lag, mutate, etc.)
