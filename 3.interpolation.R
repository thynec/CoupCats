# interpolation_functions.R

# Load necessary libraries
#library(dplyr)
#library(zoo)
#library(lubridate)
#library(forecast)

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

# Function to perform backward fill for data before the first observation
backward_fill_before_first_obs <- function(df, value_column,
                                           country_col = "ccode",
                                           year_col = "year",
                                           month_col = "month") {
  
  na_before <- sum(is.na(df[[value_column]]))
  
  df <- df %>%
    group_by(!!sym(country_col)) %>%
    arrange(!!sym(year_col), !!sym(month_col)) %>%
    mutate(
      first_obs = min(which(!is.na(!!sym(value_column)))),
      !!sym(value_column) := ifelse(
        row_number() < first_obs & is.na(!!sym(value_column)),
        !!sym(value_column)[first_obs],
        !!sym(value_column)
      )
    ) %>%
    select(-first_obs) %>%
    ungroup()
  
  na_after <- sum(is.na(df[[value_column]]))
  
  cat(sprintf(
    "\nFor column '%s', backward fill before first observed value changed %d rows.\n",
    value_column, na_before - na_after
  ))
  
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

#Function to perform AR(p) model to forecast data past last observed value
forecast_future_missing_values <- function(df, value_column, country_col = "ccode",
                                year_col = "year", month_col = "month",
                                forecast_end = Sys.Date(),
                                new_column_suffix = "_forecasted",
                                max_order = 12) {
  
  new_col <- paste0(value_column, new_column_suffix)
  df[[new_col]] <- df[[value_column]] 
  
  df <- df %>%
    mutate(date = make_date(!!sym(year_col), !!sym(month_col), 1)) %>%
    arrange(!!sym(country_col), date)
  
  df <- df %>%
    group_by(!!sym(country_col)) %>%
    group_modify(~ {
      series <- .x[[value_column]]
      dates <- .x$date
      last_date <- max(dates)
      
      forecast_months <- interval(last_date, forecast_end) %/% months(1)
      
      if (forecast_months > 0 & sum(!is.na(series)) > 1) {
        # Fit AR(p) model automatically
        fit <- tryCatch({
          auto.arima(series,
                     d = 0,             
                     max.p = max_order,
                     max.q = 0,         
                     seasonal = FALSE,  
                     stepwise = TRUE,   
                     approximation = FALSE)
        }, error = function(e) NULL)
        
        if (!is.null(fit)) {
          fc <- forecast(fit, h = forecast_months)
          new_dates <- seq(last_date %m+% months(1), by = "month", length.out = forecast_months)
          forecast_df <- tibble(
            !!country_col := unique(.x[[country_col]]),
            !!year_col := year(new_dates),
            !!month_col := month(new_dates),
            date = new_dates,
            !!new_col := as.numeric(fc$mean)
          )
          
          .x <- bind_rows(.x, forecast_df)
        }
      }
      return(.x)
    }) %>%
    ungroup() %>%
    select(-date)
  
  return(df)
}

#Function to perform forward fill for missing values after last observed value for binary data
forward_fill_binary_missing_values <- function(df, value_column, country_col = "ccode",
                                year_col = "year", month_col = "month",
                                new_column_suffix = "_ffill") {
  
  new_col <- paste0(value_column, new_column_suffix)
  df[[new_col]] <- df[[value_column]]  
  
  df <- df %>%
    mutate(date = make_date(!!sym(year_col), !!sym(month_col), 1)) %>%
    arrange(!!sym(country_col), date)
  
  df <- df %>%
    group_by(!!sym(country_col)) %>%
    group_modify(~ {

      series <- .x[[value_column]]
      
      if (sum(!is.na(series)) > 0) {
        series_ffill <- zoo::na.locf(series, na.rm = FALSE, fromLast = FALSE)
        .x[[new_col]] <- series_ffill
      }
      return(.x)
    }) %>%
    ungroup() %>%
    select(-date)
  
  return(df)
}
