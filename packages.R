#Data Sources
devtools::install_github("vdeminstitute/vdemdata") #brings in Vdem Data
install.packages("WDI") #World Development Indicators; World Bank

# Data Management
install.packages("tidyverse") # Primary package for data management
install.packages("readr") # Reads flat files (i.e., CSV)
install.packages("openxlsx") # Reads (and writes) Excel files
install.packages("haven") # Imports & exports SPSS, SAS, and Stata files
install.packages("janitor") # Cleans column names and tidies data
install.packages("Hmisc") # For labeling data. 
install.packages("labelled") # For labeling data.
install.packages("pltesim") #For setting up bctcs peace year
install.packages('readxl')
install.packages('broom')
install.packages("pltesim") #add pce years for temporal ind; implementing btscs
install.packages("R.utils") # helps bring in zipped .csv
install.packages("data.table") # helps bring in zipped .csv

# Data Visualization
install.packages("ggplot2") # Primary package for data visualization
install.packages("gganimate") # Creates animated plots (with ggplot2)
install.packages("ggfortify") # Visualizes model outputs (PCA, clustering)
install.packages("ggmosaic") # Creates mosaic plots
install.packages("ggpubr") # Enhances ggplot2 with greater details
install.packages("ggstance") # Horizontal geoms for ggplot2
install.packages("plotly") # Create interactive plots
install.packages("cowplot") #Add on to ggplot
install.packages("scales") #Used to color code significance

# Statistical Analysis and Modeling
install.packages("broom") # Tidy model outputs into data frames
install.packages("car") # Regression diagnostics and statistical methods
install.packages("DescTools") # Descriptive stats and hypothesis testing
install.packages("emmeans") # Estimated marginal means and model comparisons
install.packages("olsrr") # Linear regression diagnostics and model selection
install.packages("lme4") # Linear and generalized linear mixed-effects models
install.packages("MASS") # Various statistical models and methods
install.packages("margins") #Used for marginal effects

# Spatial Analysis
install.packages("sf") # Handle spatial data and analysis
install.packages("spData") # Spatial datasets for use with sf
install.packages("terra") # Work with raster data and spatial analysis

# Report Generation
install.packages("rmarkdown") # Dynamic report generation
install.packages("knitr") # Process and display R code/output in reports

# Model Diagnostics and Evaluation
install.packages("performance") # Evaluate model performance and diagnostics
install.packages("DHARMa") # Residual diagnostics for GLMss
install.packages("lmtest") #Linear regression diagnostics

# Data Summarization and Reporting
install.packages("gt") #used for regression tables
install.packages("gtsummary") # Summarizes regression models
install.packages("summarytools") # Quick summaries of your data
install.packages("stargazer") # Creates regression tables

# Data Export and Import
install.packages("remotes") # #install R packages from GitHub or other sources

# Miscellaneous Utilities
install.packages("gapminder") # Global development indicators and example data
install.packages("peacesciencer")  # Political science/peace studies tools
install.packages("rcompanion") # Companion functions for statistical tasks
install.packages("rgl") # 3D visualization
install.packages("WDI") # World Bank Worldwide Data Indicators 

# Data Collection and Statistics
install.packages("tigerstats") # Common statistical analysis methods
install.packages("vcd") # Visualize categorical data using association plots

# Data analysis
install.packages('aod')
