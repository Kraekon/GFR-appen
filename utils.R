#' Parse string to decimal, handling commas
#' @param x Character, numeric string (e.g., "1,23" or "1.23")
#' @return Numeric, parsed value or NA if invalid
parse_decimal <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(NA_real_)
  x <- gsub(",", ".", x)
  as.numeric(x)
}

#' Check if a number or vector of numbers is valid and above a minimum
#' @param x Numeric or numeric vector, value(s) to check
#' @param min Numeric, minimum allowed value (default 0)
#' @param inclusive Logical, whether comparison should be >= (TRUE) or > (FALSE)
#' @return Logical, TRUE if all values are valid and > min (or >= if inclusive)
is_valid_num <- function(x, min = 0, inclusive = FALSE) {
  # Accept scalar or vector numeric inputs. Return FALSE if any NA or non-numeric.
  if (is.null(x)) return(FALSE)
  if (!is.numeric(x)) return(FALSE)
  if (any(is.na(x))) return(FALSE)
  if (inclusive) {
    all(x >= min)
  } else {
    all(x > min)
  }
}

#' Validate HH:MM time format
#' @param time Character, "HH:MM"
#' @return Logical
is_valid_hhmm <- function(time) {
  if (!is.character(time) || length(time) != 1 || time == "") return(FALSE)
  grepl("^([0-1][0-9]|2[0-3]):[0-5][0-9]$", time)
}

#' Calculate time difference in minutes
#' Handles crossing midnight by treating end time as next day when appropriate.
#' @param time1 Character, start time in "HH:MM" format
#' @param time2 Character, end time in "HH:MM" format
#' @return Numeric, difference in minutes or NA if invalid
calc_time_difference_min <- function(time1, time2) {
  if (!is_valid_hhmm(time1) || !is_valid_hhmm(time2)) return(NA_real_)
  t1 <- as.POSIXct(strptime(time1, format = "%H:%M", tz = "UTC"))
  t2 <- as.POSIXct(strptime(time2, format = "%H:%M", tz = "UTC"))
  if (is.na(t1) || is.na(t2)) return(NA_real_)
  diff_min <- as.numeric(difftime(t2, t1, units = "mins"))
  # If negative, assume sample was taken the next calendar day
  if (diff_min < 0) diff_min <- diff_min + 24 * 60
  diff_min
}