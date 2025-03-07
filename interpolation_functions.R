# interpolation_functions.R

# Load necessary libraries
#library(dplyr)
#library(zoo)

# Function to check for missing values
check_missing_values <- function(df) {
  values <- colnames(df)[sapply(df, function(x) any(is.na(x)))]
  
  cat("Columns with missing values:\n")
  for (col in values) {
    na_count <- sum(is.na(df[[col]]))
    total <- nrow(df)
    pct <- na_count / total * 100
    cat(sprintf("%s has %d / %d rows (%.2f%%) missing values\n", col, na_count, total, pct))
  }
  
  exclude_columns <- c('e_asia_pacific', 'euro_cent_asia', 'LA_carrib', 'MENA', 'N_america', 'S_asia', 'Sub_africa')
  values <- setdiff(values, exclude_columns)
  
  cat("\nColumns with missing values (excluding binary columns):", values, "\n")
  return(values)
}

# Function to perform forward/backward fill
fill_missing_values <- function(df, values) {
  change_counts <- list()
  
  for (val in values) {
    # Forward fill
    na_before_ffill <- sum(is.na(df[[val]]))
    df <- df %>%
      group_by(ccode, year) %>%
      mutate(!!val := zoo::na.locf(!!sym(val), na.rm = FALSE)) %>%
      ungroup()
    na_after_ffill <- sum(is.na(df[[val]]))
    ffill_changes <- na_before_ffill - na_after_ffill
    
    # Backward fill
    na_before_bfill <- na_after_ffill
    df <- df %>%
      group_by(ccode, year) %>%
      mutate(!!val := zoo::na.locf(!!sym(val), na.rm = FALSE, fromLast = TRUE)) %>%
      ungroup()
    na_after_bfill <- sum(is.na(df[[val]]))
    bfill_changes <- na_before_bfill - na_after_bfill
    
    change_counts[[val]] <- list(ffill = ffill_changes, bfill = bfill_changes)
  }
  
  # Print fill results
  for (val in values) {
    cat(sprintf("\nFor column '%s':", val))
    cat(sprintf("\n  Forward fill (ffill) changed %d rows.", change_counts[[val]]$ffill))
    cat(sprintf("\n  Backward fill (bfill) changed %d rows.", change_counts[[val]]$bfill))
  }
  
  return(df)
}

# Function to interpolate missing values
interpolate_missing_values <- function(df, value_column, 
                                       country_col = 'ccode', 
                                       year_col = 'year', 
                                       month_col = 'month', 
                                       max_gap = 36, 
                                       method = 'linear', 
                                       new_column_suffix = '_interpolated', 
                                       order = 3) {
  new_col <- paste0(value_column, new_column_suffix)
  
  df <- df %>%
    arrange(!!sym(country_col), !!sym(year_col), !!sym(month_col)) %>%
    mutate(!!new_col := !!sym(value_column))
  
  window_size <- 2 * max_gap + 1
  
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
