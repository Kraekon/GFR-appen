# Check if a number or vector of numbers is valid and within bounds
is_valid_num <- function(x, min = -Inf, max = Inf, inclusive = TRUE) {
  if (is.null(x) || !is.numeric(x) || any(is.na(x))) {
    return(FALSE)
  }
  if (!all(is.finite(x))) {
    return(FALSE)
  }
  if (inclusive) {
    all(x >= min & x <= max)
  } else {
    all(x > min & x < max)
  }
}

# Validate HH:MM time format
is_valid_hhmm <- function(time) {
  if (!is.character(time) || length(time) != 1 || time == "") return(FALSE)
  grepl("^([0-1][0-9]|2[0-3]):[0-5][0-9]$", time)
}

# Calculate time difference in minutes from HH:MM strings
calc_time_difference_min <- function(time1_str, time2_str) {
  if (!is_valid_hhmm(time1_str) || !is_valid_hhmm(time2_str)) return(NA)
  time1 <- as.POSIXct(time1_str, format = "%H:%M")
  time2 <- as.POSIXct(time2_str, format = "%H:%M")
  if (is.na(time1) || is.na(time2)) return(NA)
  as.numeric(difftime(time2, time1, units = "mins"))
}

# Format decimal hours to hours and minutes
format_hours_minutes <- function(decimal_hours) {
  if (!is_valid_num(decimal_hours)) return("NA")
  hours <- floor(decimal_hours)
  minutes <- round((decimal_hours - hours) * 60)
  sprintf("%d h %d min", hours, minutes)
}