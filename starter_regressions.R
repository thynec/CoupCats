rm(list=ls())
#setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #clay at home

#load packages: if you use new packages, try to remember to add them to the packages on github. Otherwise, just flag them and I'll do it.
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R")

#load libraries: if you add new libraries, try to remember to add them to github. Otherwise, just flag them and I'll do it.
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") #will add to master script after this one

#CT note to user on 03/02/25: Keep using the fake data for now, though we're close to having good data. As I get it cleaned and ready for analysis, I'll just update the .csv on the main branch and you can load it from there. Just use the following commmand...
base_data <- read_csv("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/base_data.csv") #I see this note but we had already started using the real data, and I think it is reasonable to use this if we were going to use fake data anyway
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#logit with coup attempt as dv and population total, median age, military expenditure (total and percent of GDP)
coup_logit <- glm(`coup_attempt` ~ `pop` + `median_age` + `milex` + `milper`, data = base_data, family = 'binomial')
summary(coup_logit)

#probability change
predicted_logit <- predict(coup_logit, type = 'response') #predicted probability
mfxL <- margins(coup_logit, type = 'response') #marginal effects
summary(mfxL) # R rounds to 0
print(mfxL, digits = 6) # do this to see actual values



# Logit table using GT, error message on color scale, fix rounding and which "statistic"
# Convert model summary into a tidy dataframe
model_summary <- tidy(coup_logit) 
# Create the table
formatted_table <- gt(model_summary)
# Format numerical columns
formatted_table <- fmt_number(
  formatted_table,
  columns = c("estimate", "std.error", "statistic", "p.value"),
  decimals = 4) #may want more or scientific notation
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red
formatted_table <- data_color(
  formatted_table,
  columns = "p.value",
  colors = col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Title and Subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Logistic Regression Summary**"),
  subtitle = md("*Effect of Social and Military variables on Coup Attempts")) 
# Footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")

print(formatted_table)



# Marginal effects table using GT
# Convert to a tidy dataframe
mfxL_df <- as.data.frame(summary(mfxL))
# Create the table
formatted_table <- gt(mfxL_df)
# Format numerical columns for readability
formatted_table <- fmt_number(
  formatted_table,
  columns = c("AME", "SE", "z", "p", "lower", "upper"),
  decimals = 4)
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red (p < 0.05)
formatted_table <- data_color(
  formatted_table,
  columns = "p",
  colors = scales::col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Add title and subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Marginal Effects Summary**"),
  subtitle = md("*Average Marginal Effects from Logistic Regression*"))
# Add footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")
# Print the formatted table
print(formatted_table)



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#probit with coup attempt as dv and population total, median age, military expenditure (total and percent of GDP)
coup_probit <- glm(`coup_attempt` ~ `pop` + `median_age` + `milex` + `milper`, data = base_data, family = binomial(link = 'probit'))
summary(coup_probit)

#predicted probabilities for probit
predicted_probit <- predict(coup_probit, type = 'response') #predicted probabilites
mfxP <- margins(coup_probit) #marginal efffects
summary(mfxP) # R rounds to 0
print(mfxP, digits = 6) # do this to see actual values




# Probit table using GT, error message about color scale, fix rounding and which "statistic"
# Convert model summary into a tidy dataframe
model_summary <- tidy(coup_probit) 
# Create the table
formatted_table <- gt(model_summary)
# Format numerical columns
formatted_table <- fmt_number(
  formatted_table,
  columns = c("estimate", "std.error", "statistic", "p.value"),
  decimals = 4)
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red
formatted_table <- data_color(
  formatted_table,
  columns = "p.value",
  colors = col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Title and Subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Probit Regression Summary**"),
  subtitle = md("*Effect of Social and Military Variables on Coup Attemps*")) #should probably change this subtitle
# Footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")

print(formatted_table)


# Marginal effects table using GT
# Convert to a tidy dataframe
mfxP_df <- as.data.frame(summary(mfxL))
# Create the table
formatted_table <- gt(mfxL_df)
# Format numerical columns for readability
formatted_table <- fmt_number(
  formatted_table,
  columns = c("AME", "SE", "z", "p", "lower", "upper"),
  decimals = 4)
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red (p < 0.05)
formatted_table <- data_color(
  formatted_table,
  columns = "p",
  colors = scales::col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Add title and subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Marginal Effects Summary**"),
  subtitle = md("*Average Marginal Effects from Probit Regression*"))
# Add footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")
# Print the formatted table
print(formatted_table)


#-------------------------------------------------------------------------------------------------------------------
#DIAGNOSTICS (both models)

#Logit
#pearson residuals
pearson_residuals_logit <- residuals(coup_logit, type = "pearson")
pearson_residuals_logit

#plotted pearson residuals
#needs work

#find Cook's Distance (for influential outliers)
logit_cooksD = cooks.distance(coup_logit)

#plotting Cook's Distance (base, need to add clarity and labels)
#needs work

#Probit
#pearson residuals (preferred)
pearson_residuals_probit <- residuals(coup_probit, type = "pearson")
pearson_residuals_probit

#plotting pearson residuals
#needs work 

#find Cook's Distance (for influential outliers)
probit_cooksD = cooks.distance(coup_probit)

#plotting Cook's Distance (base, need to add clarity and labels)
#needs work

#Other diagnositc tests need adding
