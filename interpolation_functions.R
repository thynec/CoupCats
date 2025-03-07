# Script to linearly interpolate data, should be used at the very end #

#install.packages("zoo")   # Flag to make sure its in packages
#install.packages("dplyr") # Flag to make sure its in packages

library(dplyr) # Flag to make sure its in libraries
library(zoo) # Flag to make sure its in libraries

# Load Dataset
url <- "https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/base_data.csv"
df <- read.csv(url)
head(df)

# Check for missing values
values <- colnames(df)[sapply(df, function(x) any(is.na(x)))]

cat("Columns with missing values:\n")
for (col in values) {
  na_count <- sum(is.na(df[[col]]))
  total <- nrow(df)
  pct <- na_count / total * 100
  cat(sprintf("%s has %d / %d rows (%.2f%%) missing values\n", col, na_count, total, pct))
}

# By visual inspection of the dataset, we can exclude the following because of binary values (not ideal for interpolation)
exclude_columns <- c('e_asia_pacific', 'euro_cent_asia', 'LA_carrib', 'MENA', 'N_america', 'S_asia', 'Sub_africa')
values <- setdiff(values, exclude_columns)

cat("\nColumns with missing values (excluding binary columns):", values, "\n")

# Track changes from forward/backward fill
change_counts <- list()

for (val in values) {
  # Count missing values before forward fill
  na_before_ffill <- sum(is.na(df[[val]]))
  
  # Apply forward fill (ffill) within each group
  df <- df %>%
    group_by(ccode, year) %>%
    mutate(!!val := zoo::na.locf(!!sym(val), na.rm = FALSE)) %>%
    ungroup()
  
  # Count missing values after forward fill
  na_after_ffill <- sum(is.na(df[[val]]))
  
  # Update change count for ffill
  ffill_changes <- na_before_ffill - na_after_ffill
  
  # Count missing values before backward fill
  na_before_bfill <- na_after_ffill
  
  # Apply backward fill (bfill) within each group
  df <- df %>%
    group_by(ccode, year) %>%
    mutate(!!val := zoo::na.locf(!!sym(val), na.rm = FALSE, fromLast = TRUE)) %>%
    ungroup()
  
  # Count missing values after backward fill
  na_after_bfill <- sum(is.na(df[[val]]))
  
  # Update change count for bfill
  bfill_changes <- na_before_bfill - na_after_bfill
  
  change_counts[[val]] <- list(ffill = ffill_changes, bfill = bfill_changes)
}

# Print the results
for (val in values) {
  cat(sprintf("\nFor column '%s':", val))
  cat(sprintf("\n  Forward fill (ffill) changed %d rows.", change_counts[[val]]$ffill))
  cat(sprintf("\n  Backward fill (bfill) changed %d rows.", change_counts[[val]]$bfill))
}

# Interpolation function
interpolate_missing_values <- function(df, value_column, 
                                       country_col = 'ccode', 
                                       year_col = 'year', 
                                       month_col = 'month', 
                                       max_gap = 36, 
                                       method = 'linear', 
                                       new_column_suffix = '_interpolated', 
                                       order = 3) {
  """
  Interpolates missing values within country groups with validity checks.
  
  Parameters:
  df (data.frame): Input dataframe containing the data
  value_column (str): Column name with values to interpolate
  country_col (str): Column name containing country codes (default 'ccode')
  year_col (str): Column name for year (default 'year')
  month_col (str): Column name for month (default 'month')
  max_gap (int): Maximum allowed gap (in months) for interpolation (default 36)
  method (str): Interpolation method (default 'linear')
  new_column_suffix (str): Suffix for new interpolated column (default '_interpolated')
  order (int): Order for spline interpolation (default 3)
  
  Returns:
  data.frame: DataFrame with new interpolated columns
  """
  
  # Create new column name
  new_col <- paste0(value_column, new_column_suffix)
  
  # Sort data by country, year, and month
  df <- df %>%
    arrange(!!sym(country_col), !!sym(year_col), !!sym(month_col)) %>%
    mutate(!!new_col := !!sym(value_column))
  
  # Calculate window size based on max_gap
  window_size <- 2 * max_gap + 1
  
  # Group by country and process each group
  df <- df %>%
    group_by(!!sym(country_col)) %>%
    mutate(
      na_mask = is.na(!!sym(value_column)),
      valid_neighbors = zoo::rollapply(
        data = !is.na(!!sym(value_column)), 
        width = window_size, 
        FUN = sum, 
        align = "center", 
        partial = TRUE, 
        fill = 0
      ),
      interpolate_mask = na_mask & (valid_neighbors >= 1),
      interpolated = if (method %in% c('spline', 'polynomial')) {
        zoo::na.spline(!!sym(value_column), na.rm = FALSE, method = "natural")
      } else if (method == 'linear') {
        zoo::na.approx(!!sym(value_column), na.rm = FALSE)
      } else {
        !!sym(value_column)
      },
      !!new_col := ifelse(interpolate_mask, interpolated, !!sym(new_col))
    ) %>%
    select(-na_mask, -valid_neighbors, -interpolate_mask, -interpolated) %>%
    ungroup()
  
  return(df)
}

# Apply interpolation
for (val in values) {
  df <- interpolate_missing_values(df = df, value_column = val, max_gap = 36, method = 'spline', order = 3) # Max gap = X months
}

# For columns in values and their interpolated versions, print the number of NA values compared to total number of rows
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
