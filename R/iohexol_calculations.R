#' Convert Iodine concentration to Iohexol concentration
#'
#' This function now correctly looks up the Iohexol concentration based on the
#' selected Iodine concentration (the product name, e.g., "Omnipaque 300").
#' The lookup table is now complete as per your specification.
iodine_to_iohexol <- function(iodine_mg_ml) {
  if (!is_valid_num(iodine_mg_ml)) return(NA)
  
  # The input 'iodine_mg_ml' is treated as the key for the lookup
  # Values are the actual Iohexol mg/mL for that product.
  switch(as.character(iodine_mg_ml),
         "350" = 755,
         "300" = 647,
         "240" = 518,
         "200" = 431,
         "180" = 388,
         "140" = 302,
         NA # Default case for invalid input
  )
}

#' Jacobsson's formula for EECV
#' Returns EECV in milliliters (mL).
jacobsson_eecv <- function(sex, weight) {
  if (sex == "Man") {
    eecv <- 166 * weight + 2490
  } else {
    eecv <- 95 * weight + 6170
  }
  return(eecv)
}

#' One-point iohexol GFR calculation
iohexol_one_point_gfr <- function(sex, weight, height, dose_mg, plasma_mg_L, t_min, bsa_formula) {
  if (!is_valid_num(c(dose_mg, plasma_mg_L, t_min)) || t_min <= 0) {
    return(list(gfr = NA, gfr_adj = NA, t_opt_hours = NA))
  }
  
  bsa <- bsa_select(weight, height, sex, bsa_formula)
  if (!is_valid_num(bsa)) return(list(gfr = NA, gfr_adj = NA, t_opt_hours = NA))
  
  eecv_ml <- jacobsson_eecv(sex, weight)
  
  # 1. Calculate initial uncorrected clearance (Clv)
  log_arg_clv <- log((1000 * dose_mg) / (eecv_ml * plasma_mg_L))
  cl_uncorrected <- (1 / ((t_min / eecv_ml) + 0.0016)) * log_arg_clv
  
  # 2. Calculate m and v'
  m <- 0.991 - (0.00122 * cl_uncorrected)
  v_prime <- eecv_ml / m
  
  # 3. Calculate final, corrected absolute GFR using v'
  log_arg_gfr <- log((1000 * dose_mg) / (v_prime * plasma_mg_L))
  gfr <- (1 / ((t_min / v_prime) + 0.0016)) * log_arg_gfr
  
  # Calculate relative GFR
  gfr_adj <- if (is_valid_num(bsa)) gfr * (1.73 / bsa) else NA
  
  # Calculate optimal sampling time
  t_opt_hours <- if(is_valid_num(gfr) && gfr > 0) (v_prime / gfr) / 60 else NA
  
  return(list(gfr = gfr, gfr_adj = gfr_adj, t_opt_hours = t_opt_hours))
}

#' Slope-intercept GFR (multi-point) with Brøchner-Mortensen correction
slope_intercept_gfr <- function(times_min, concs_num, dose_mg, bsa) {
  if (length(times_min) != length(concs_num) || length(times_min) < 2) return(list(gfr=NA, gfr_adj=NA))
  
  df <- data.frame(time = times_min, conc = concs_num)
  fit <- lm(log(conc) ~ time, data = df)
  
  if (any(is.na(coef(fit)))) return(list(gfr = NA, gfr_adj = NA))
  
  intercept <- coef(fit)[1]
  slope <- coef(fit)[2]
  
  if (slope >= 0) return(list(gfr = NA, gfr_adj = NA))
  
  k_pos <- -slope
  c0 <- exp(intercept)
  
  # dose_mg is in mg, c0 is in mg/L, so (dose_mg / c0) is in L.
  # k_pos is in 1/min. The result is in L/min. Multiply by 1000 for mL/min.
  cl_uncorrected <- (dose_mg / c0) * k_pos * 1000
  
  # Apply Brøchner-Mortensen correction
  gfr <- 0.99078 * cl_uncorrected - 0.001218 * (cl_uncorrected^2)
  
  gfr_adj <- if (is_valid_num(bsa)) gfr * (1.73 / bsa) else NA
  
  return(list(gfr = gfr, gfr_adj = gfr_adj))
}