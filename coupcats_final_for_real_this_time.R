#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building domestic political variables

#1. clear all
rm(list = ls())
#2. set working directory
#setwd("~/R/coupcats") # Set working file. 
#setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
#setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/teaching/1.coupcast/TEK_S26/git_2026.03.13") #clay at work
#3. install packages
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") #5. build baseline
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#2.a.domestic political
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.a.base_data.csv.gz"
base_data.2a <- fread(url)
rm(url)
#2.b.domestic economic
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.b.base_data.csv.gz"
base_data.2b <- fread(url)
rm(url)
#2.c.political instability
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.c.base_data.csv.gz"
base_data.2c <- fread(url)
rm(url)
#2.d.military variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.d.base_data.csv.gz"
base_data.2d <- fread(url)
rm(url)
#2.e.international variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.e.base_data.csv.gz"
base_data.2e <- fread(url)
rm(url)

base_data <- base_data.2a
rm(base_data.2a)
base_data.2b <- base_data.2b %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -pce, -pce2, -pce3, -pce_succ, -pce2_succ, -pce3_succ, -cw, -cw_onset, -pce_cw, -pce2_cw, -pce3_cw, -protests_DV)
base_data <- base_data %>%
  left_join(base_data.2b, by=c("ccode", "year", "month"))
rm(base_data.2b)
base_data.2c <- base_data.2c %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -pce, -pce2, -pce3, -pce_succ, -pce2_succ, -pce3_succ, -cw, -cw_onset, -pce_cw, -pce2_cw, -pce3_cw, -protests)
base_data <- base_data %>%
  left_join(base_data.2c, by=c("ccode", "year", "month"))
rm(base_data.2c)    
base_data.2d <- base_data.2d %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -pce, -pce2, -pce3, -pce_succ, -pce2_succ, -pce3_succ, -cw, -cw_onset, -pce_cw, -pce2_cw, -pce3_cw, -protests)
base_data <- base_data %>%
  left_join(base_data.2d, by=c("ccode", "year", "month"))
rm(base_data.2d)    
base_data.2e <- base_data.2e %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -pce, -pce2, -pce3, -pce_succ, -pce2_succ, -pce3_succ, -cw, -cw_onset, -pce_cw, -pce2_cw, -pce3_cw, -protests)
base_data <- base_data %>%
  left_join(base_data.2e, by=c("ccode", "year", "month"))
rm(base_data.2e)

# ------------------------------------------------------------#
# 1. Build time index
# ------------------------------------------------------------#
df <- base_data %>%
  arrange(ccode, year, month) %>%
  mutate(time_id = year * 12 + month)

#not trusting data before 1960 due to extrapolation; let's end it
df <- df %>%
  filter(year>=1960)

# ---------------------------------------------------------------------#
# 2. Drop unused OG variables (only if they exist)
# ---------------------------------------------------------------------#
df <- df %>% select(-any_of(c(
  "wom_civlib_OG", "gini_OG", "gini_wdi_OG", "gini_wiid_OG",
  "gini_swiid_OG", "vdem_gdppc_OG", "hc_OG", "oda_OG", "nr_rents_OG", "debt_OG",
  "wdi_gdppc_OG", "stability_OG", "mobilization_OG", "mobil_conc_OG", "acled_OG",
  "acledl", "acledlz", "milex_spliced_OG", "milper_spliced_OG", "trade_glob_OG",
  "pol_glob_OG", "gender_parity_OG", "women_polemp_OG", "interpersonal_glob_OG",
  "fdi_OG", "arrivals_OG", "ltrade_OG", "ldtrade_OG", "full", "associate",
  "observer", "full_OG", "associate_OG", "observer_OG", "IOs_sum_OG"
)))


# ------------------------------------------------------------#
# 3. Define IV blocks
# ------------------------------------------------------------#
leader_IVs <- c("numleaders_10yr") #"Leader_age", "prop_milit_career", "Leader_duration"su
regime_IVs <- c("polyarchy") #"polyarchy", "polyarchy2"
milit_leader_IVs <- c("milreg_prop") #"milit", "milreg", "milreg_prop",,  "prop_milit_career"
milit_IVs <- c("milper_spliced", "mutiny6") #"solqual", "milex_spliced", "milper_spliced", , "mutiny12", "mutiny6"
gender_IVs <- c("wom_polpart") #, "women_polemp", "wom_civlib", "gender_parity"
econ_IVs <- c("ch_gdppc") #, "hc", "gini", "oda", "nr_rents", "debt"
stability_IVs <- c("mobilization") #"stability_WB", , "mobil_conc", , "cw_lag", "brd", "protests"
glob_IVs <- c("signal", "trade_glob",   #"mid_primary", "pol_glob",
              "cold") #"interpersonal_glob", , "ldtrade", "ltrade"
IO_IVs <- c("") #, , "IOs_sum", "defense_alliance_active"
coup_auto <- c("pce", "pce2", "pce3")

ivs <- c(
  leader_IVs,
  regime_IVs,
  milit_leader_IVs,
  milit_IVs,
  gender_IVs,
  econ_IVs,
  stability_IVs,
  glob_IVs,
  coup_auto
)

# Build formula
fml <- as.formula(
  paste("coup_attempt ~", paste(ivs, collapse = " + "))
)
# Run linear model
coup_mod <- lm(fml, data = df, cluster=~ccode)

# View results
summary(coup_mod)

df$yhat <- predict(coup_mod, newdata=df, type="response")
years <- df %>%
  filter(!is.na(yhat))
summary(years$year) #these are the years that were included in the last regression; we want this to be 2025
months <-years %>%
  filter(year==2026)
summary(months$month) #this is the last month in the regression, assuming we get it updated to 2025; we want this to be 3 or 4 (March or April)
rm(years, months)
missing <- df %>%
  filter(year==2026 & month==4) %>%
  relocate(yhat) %>%
  filter(is.na(yhat))
table(missing$country) #these are the countries that won't show up in our map
df <- df %>%
  select(-yhat)

#------------------------------------------------------------------------------------------------#  
#Look at top-10 idea to show that we have a good model
#------------------------------------------------------------------------------------------------#  
outcome <- df %>%
  mutate(prediction_prob = predict(coup_mod, newdata=df, type="response")) %>%
  select(ccode, year, month, coup_attempt, prediction_prob) %>%
  filter(!is.na(prediction_prob))
outcome <- outcome %>%
  group_by(year) %>%
  mutate(mean=mean(prediction_prob)) %>%
  mutate(sd=sd(prediction_prob)) %>%
  mutate(z=(prediction_prob-mean)/sd) %>%
  filter(coup_attempt==1) %>%
  mutate(percentile=pnorm(z)*100)
tot <- nrow(outcome)
p90 <- outcome %>%
  filter(percentile>90)
p90 <- nrow(p90)/tot #so 49.3% of coup attempts happened in states we had ranked in 90+ percentile
outcome <- outcome %>%
  arrange(year, -prediction_prob) %>%
  group_by(year) %>%
  mutate(rank=row_number()) %>%
  ungroup()
outcome <- outcome %>%
  filter(coup_attempt==1) %>%
  mutate(rank=(round(rank, 1))) %>%
  mutate()

#------------------------------------------------------------------------------------------------#  
#Pretty table of baseline model - NEED to replace var names with labels at some point
#------------------------------------------------------------------------------------------------#  

# Marginal effects 
mfxL <- margins(coup_mod, type = 'response') # Effect of each predictor expressed as a probability (Average Marginal Effect)
summary(mfxL) # R rounds to 0 (virtually no effect)--remember, coups are rare events! 
print(mfxL, digits = 6) 

# Calculate DFBETAs
dfbetas_values <- dfbetas(coup_mod) # Large DFBETAs indicate a specific observation (row) has a strong influence.

# Find observations with any DFBETA greater than 2/sqrt(n). 
influential_obsL <- apply(abs(dfbetas_values), 1, function(x) any(x > 2)) # 2 is arbitrary; common threshold for influence.

# Display influential observations
which(influential_obsL) # No single observation significantly alters the model. 
rm(dfbetas_values, influential_obsL)

# Building logit table
model_summary <- tidy(coup_mod) 
formatted_table <- gt(model_summary) 
formatted_table <- fmt_number(
  formatted_table,
  columns = c("estimate", "std.error", "statistic", "p.value"),
  decimals = 4) 
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
formatted_table <- data_color(
  formatted_table,
  columns = "p.value",
  fn = col_numeric(
    palette = c("blue", "black"),
    domain = c(0, 0.05)))
formatted_table <- tab_header(
  formatted_table,
  title = md("**Logistic Regression Summary**"),
  subtitle = md("*Effect of Social and Military variables on Coup Attempts")) 
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in blue.")
gtsave(formatted_table, "regression_table.html")
webshot2::webshot("regression_table.html", "regression_table.pdf")
print(formatted_table)
rm(model_summary, formatted_table)

# Building marginal effects table. 
mfxL_df <- as.data.frame(summary(mfxL))
formatted_table <- gt(mfxL_df)
formatted_table <- fmt_number(
  formatted_table,
  columns = c("AME", "SE", "z", "p", "lower", "upper"),
  decimals = 4)
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
formatted_table <- data_color(
  formatted_table,
  columns = "p",
  colors = scales::col_numeric(
    palette = c("blue", "black"),
    domain = c(0, 0.05)))
formatted_table <- tab_header(
  formatted_table,
  title = md("**Marginal Effects Summary**"),
  subtitle = md("*Average Marginal Effects from Logistic Regression*"))
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in blue.")
gtsave(formatted_table, "marginal_effects_table.html")
print(formatted_table)
rm(formatted_table)

#------------------------------------------------------------------------------------------------#  
#DIAGNOSTICS 
#------------------------------------------------------------------------------------------------#  
model_data <- df[complete.cases(df[, c("coup_attempt", ivs), with = FALSE]), ]

#Accuracy Logit
predicted_logit <- predict(coup_mod, type = 'response') #predicted probability

# Convert probabilities to binary class labels
predicted_classes <- ifelse(predicted_logit > 0.0045, 1, 0)
predicted_classes <- factor(predicted_classes, levels = c(0, 1))  # Specify levels 0 and 1
actual_classes <- factor(model_data$coup_attempt, levels = c(0, 1))  # Ensure actual labels have the same levels
# Create confusion matrix
conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(model_data$coup_attempt))

# View confusion matrix
conf_matrix

# Pretty table of confusion matrix
cm_table <- as.data.frame(conf_matrix$table)
colnames(cm_table) <- c("Predicted", "Actual", "Freq")

# Calculate percentages
cm_table$Pct <- round(cm_table$Freq / sum(cm_table$Freq) * 100, 1)

# Label correct vs incorrect
cm_table$correct <- ifelse(
  (cm_table$Predicted == 1 & cm_table$Actual == 1) | 
    (cm_table$Predicted == 0 & cm_table$Actual == 0), 
  "Correct", "Incorrect")

# Relabel factor levels for display
cm_table$Predicted <- factor(cm_table$Predicted, levels = c(1, 0), labels = c("Coup", "No Coup"))
cm_table$Actual <- factor(cm_table$Actual, levels = c(0, 1), labels = c("No Coup", "Coup"))

# Plot
ggplot(cm_table, aes(x = Actual, y = factor(Predicted, levels = c("No Coup", "Coup")), 
                     fill = correct, alpha = Pct)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(aes(label = paste0(Freq, "\n(", Pct, "%)")), size = 5, alpha = 1) +
  scale_fill_manual(values = c("Correct" = "#6baed6", "Incorrect" = "#fb6a4a")) +
  scale_alpha_continuous(range = c(0.3, 0.9), guide = "none") +
  labs(
    title = "Confusion Matrix",
    x = "Actual Coup",
    y = "Predicted Coup",
    fill = "Prediction"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid = element_blank()
  )

