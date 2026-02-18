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
  
  # Order data properly first
  df <- df[order(df[[country_col]],
                 df[[year_col]],
                 df[[month_col]]), ]
  
  # Split by country
  split_data <- split(df, df[[country_col]])
  
  # Apply backward fill within each country
  split_data <- lapply(split_data, function(group_df) {
    
    x <- group_df[[value_column]]
    
    if (!all(is.na(x))) {
      first_obs <- which(!is.na(x))[1]
      
      if (first_obs > 1) {
        x[1:(first_obs - 1)] <- x[first_obs]
      }
      
      group_df[[value_column]] <- x
    }
    
    return(group_df)
  })
  
  # Recombine
  df <- do.call(rbind, split_data)
  
  na_after <- sum(is.na(df[[value_column]]))
  
  cat(sprintf(
    "\nFor column '%s', backward fill before first observed value changed %d rows.\n",
    value_column, na_before - na_after
  ))
  
  return(df)
}

#fill interior binary gaps when end points match
fill_binary_interior_gaps <- function(df, value_column,
                                      country_col = "ccode",
                                      year_col = "year",
                                      month_col = "month") {
  
  # Order properly
  df <- df[order(df[[country_col]],
                 df[[year_col]],
                 df[[month_col]]), ]
  
  split_data <- split(df, df[[country_col]])
  
  split_data <- lapply(split_data, function(group_df) {
    
    x <- group_df[[value_column]]
    
    if (sum(!is.na(x)) > 1) {
      
      non_na_index <- which(!is.na(x))
      
      for (i in seq_along(non_na_index)[-length(non_na_index)]) {
        
        start <- non_na_index[i]
        end   <- non_na_index[i + 1]
        
        if (end - start > 1) {
          
          gap_length <- end - start - 1
          left_val   <- x[start]
          right_val  <- x[end]
          
          # Case 1: bounds match (0-0 or 1-1)
          if (left_val == right_val) {
            x[(start + 1):(end - 1)] <- left_val
          } else {
            # Case 2: bounds differ (0-1 or 1-0)
            
            half_point <- floor(gap_length / 2)
            
            # First half
            if (half_point > 0) {
              x[(start + 1):(start + half_point)] <- left_val
            }
            
            # Second half
            x[(start + half_point + 1):(end - 1)] <- right_val
          }
        }
      }
    }
    
    group_df[[value_column]] <- x
    return(group_df)
  })
  
  df <- do.call(rbind, split_data)
  
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
forward_fill_after_last_obs <- function(df, value_column,
                                        country_col = "ccode",
                                        year_col = "year",
                                        month_col = "month",
                                        new_column_suffix = "_ffill") {
  
  new_col <- paste0(value_column, new_column_suffix)
  df[[new_col]] <- df[[value_column]]
  
  # Order properly
  df <- df[order(df[[country_col]],
                 df[[year_col]],
                 df[[month_col]]), ]
  
  split_data <- split(df, df[[country_col]])
  
  split_data <- lapply(split_data, function(group_df) {
    
    x <- group_df[[value_column]]
    
    if (!all(is.na(x))) {
      last_obs <- tail(which(!is.na(x)), 1)
      
      if (last_obs < length(x)) {
        x[(last_obs + 1):length(x)] <- x[last_obs]
      }
      
      group_df[[new_col]] <- x
    }
    
    return(group_df)
  })
  
  df <- do.call(rbind, split_data)
  
  return(df)
}
