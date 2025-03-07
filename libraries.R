#Data sources
library(vdemdata) #Vdem
library(WDI) #World Development Indicators; World Bank

# Data Management
library(tidyverse) # Primary package for data management
library(readr) # Reads flat files (i.e., CSV)
library(openxlsx) # Reads (and writes) Excel files
library(haven) # Imports & exports SPSS, SAS, and Stata files
library(janitor) # Cleans column names and tidies data
library(Hmisc) # For labeling data. 
library(labelled) # For labeling data.
library(readxl) # To read excel sheets.
library(pltesim) # To set up btscs; see https://www.rdocumentation.org/packages/DAMisc/versions/1.7.2/topics/btscs
library(broom)

# Data Visualization
library(ggplot2) # Primary package for data visualization
library(gganimate) # Creates animated plots (with ggplot2)
library(ggfortify) # Visualizes model outputs (PCA, clustering)
library(ggmosaic) # Creates mosaic plots
library(ggpubr) # Enhances ggplot2 with greater details
library(ggstance) # Horizontal geoms for ggplot2
library(plotly) # Create interactive plots
library(cowplot) #ggplot add on
library(scales) #color code significance level

# Statistical Analysis and Modeling
library(broom) # Tidy model outputs into data frames
library(car) # Regression diagnostics and statistical methods
library(DescTools) # Descriptive stats and hypothesis testing
library(emmeans) # Estimated marginal means and model comparisons
library(olsrr) # Linear regression diagnostics and model selection
library(lme4) # Linear and generalized linear mixed-effects models
library(MASS) # Various statistical models and methods
library(margins) #calculate marginal effects

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
library(lmtest) #Used for linear regression diagnostics

# Data Summarization and Reporting
library(gtsummary) # Summarizes regression models
library(summarytools) # Quick summaries of your data
library(stargazer) # Creates regression tables
library(gt) #Used for regression tables

# Data Export and Import
library(remotes) # Install R packages from GitHub or other sources

# Miscellaneous Utilities
library(gapminder) # Global development indicators and example data
library(peacesciencer)  # Political science/peace studies tools
library(rcompanion) # Companion functions for statistical tasks
library(rgl) # 3D visualization
library(WDI) # Loads World Bank data. 
library(vdemdata) # Loads V-Dem data. 
library(WDI) # World Bank Worldwide Data Indicators 

# Data Collection and Statistics
library(tigerstats) # Common statistical analysis methods
library(vcd) # Visualize categorical data using association plots

# Data analysis
library(aod)
