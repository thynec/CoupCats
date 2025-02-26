#get packages
install.packages('aod')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('readxl')
install.packages('broom')
install.packages("httr")
install.packages("readr")
install.packages("margins")
#libraries
library(aod)
library(ggplot2)
library(dplyr)
library(readxl)
library(broom)
library(httr)
library(readr)
library(margins)
'Notes: 1. Need to look at z scores of marginal effects for both models
2. Need to graph predicted probability change for each variable (for example, change in probability
of coup_attempted for median age. Need plots for each individual variable)
3. Need to make output tables that dont just show up in R console (for original model and marginal effects)
4. Need to change plots for both models residuals, I believe it has something to do with the number of observations
changing from the fake data set. See if plotting this is reasonable or if we need to change something (probably)
5. Cooks D needs labels for the observation points'
# URL for the raw file
url <- "https://raw.githubusercontent.com/seque1204/TEK300/main/emma_data.xlsx"

# You can use httr to read the file directly from the URL
temp_file <- tempfile(fileext = ".xlsx")
download.file(url, temp_file, mode = "wb")  # Download the file to a temporary location

# Now use readxl to read the downloaded file
coup_data <- read_xlsx(temp_file)

#Read the CSV data into R
head(coup_data)


#logit with coup as dv and age/population variables 
mylogit <- glm(`coup_attempted` ~ `popln_tot` + `age0_14` + `age15_64` + `age65plus` + `median_age`, data = coup_data, family = 'binomial')
summary(mylogit)#R logit output

#probability change
predicted_logit <- predict(mylogit, type = 'response')
marginal_effectsL <- margins(mylogit)
summary(marginal_effectsL) # R rounds to 0
print(marginal_effectsL, digits = 6) # do this to see actual values

#residuals
logit_residuals <- residuals(mylogit)
logit_residuals

#pearson residuals
LFitted <- fitted(mylogit)
pearson_residuals_logit <- residuals(mylogit, type = "pearson")
pearson_residuals_logit

#plotted pearson residuals
ggplot(coup_data, aes(x = 1:249, y = pearson_residuals_logit)) +
  geom_point(color = "blue") +  # Scatter plot of residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Horizontal line at y = 0
  labs(title = "Pearson Residuals from Logit Model",
       x = "Observations", y = "Pearson Residuals") +
  theme_minimal()

#find Cook's Distance (for influential outliers)
logit_cooksD = cooks.distance(mylogit)

#plotting Cook's Distance (base, need to add clarity and labels)
plot(logit_cooksD, type = "p", main = "Cook's Distance (Logit)", ylab = "Cook's Distance", xlab = "Observation Index")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

#probit with same variables 
myprobit <- glm(`coup_attempted` ~`popln_tot` + `age0_14` + `age15_64` + `age65plus` + `median_age`, data = coup_data, family = binomial(link = 'probit'))
summary(myprobit)


#predicted probabilities for probit
predicted_probit <- predict(myprobit, type = 'response')
marginal_effectsP <- margins(myprobit)
summary(marginal_effectsP) # R rounds to 0
print(marginal_effectsP, digits = 6) # do this to see actual values

#residuals
Fitted <- fitted(myprobit)
probit_residuals <- residuals(myprobit)
probit_residuals

#pearson residuals (preferred)
pearson_residuals_probit <- residuals(myprobit, type = "pearson")
pearson_residuals_probit

#plotting pearson residuals
ggplot(coup_data, aes(x = 1:249, y = pearson_residuals_probit)) +
  geom_point(color = "blue") +  # Scatter plot of residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Horizontal line at y = 0
  labs(title = "Pearson Residuals from Probit Model",
       x = "Observations", y = "Pearson Residuals") +
  theme_minimal()

#find Cook's Distance (for influential outliers)
probit_cooksD = cooks.distance(myprobit)

#plotting Cook's Distance (base, need to add clarity and labels)
plot(probit_cooksD, type = "p", main = "Cook's Distance (Probit)", ylab = "Cook's Distance", xlab = "Observation Index")
