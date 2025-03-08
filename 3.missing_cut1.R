#### Filling in NAs below, committed on 3/7 ####

# Source the interpolation functions script
source("interpolation_functions.R")

# Load the dataset
url <- "https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/base_data.csv"
df <- read.csv(url)

# Check for missing values
values <- check_missing_values(df)

# Fill missing values using forward/backward fill
df <- fill_missing_values(df, values)

# Interpolate missing values
for (val in values) {
  df <- interpolate_missing_values(df, val, max_gap = 36, method = 'spline', order = 3)
}

# Check remaining missing values
cat("\nFinal missing values check:\n")
for (col in c(values, paste0(values, '_interpolated'))) {
  if (col %in% colnames(df)) {
    na_count <- sum(is.na(df[[col]]))
    if (na_count > 0) {
      total <- nrow(df)
      pct <- na_count / total * 100
      cat(sprintf("%s has %d / %d rows (%.2f%%) missing values\n", col, na_count, total, pct))
    }
  }
}

#### End of Commit ####