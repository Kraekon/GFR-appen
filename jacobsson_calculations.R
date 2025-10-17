#' Iohexol concentration conversion table
#' @return Data frame with Omnipaque iodine concentrations and corresponding iohexol concentrations
iohexol_conversion_table <- function() {
  data.frame(
    iodine_mg_ml = c(140, 180, 200, 240, 300, 350),
    iohexol_mg_ml = c(302, 388, 431, 518, 647, 755)
  )
}

#' Convert iodine concentration to iohexol concentration
#' @param iodine_mg_ml Numeric, iodine concentration in mg/mL
#' @return Numeric, iohexol concentration in mg/mL or NA if invalid
iodine_to_iohexol <- function(iodine_mg_ml) {
  if (!is_valid_num(iodine_mg_ml)) return(NA_real_)
  conv_table <- iohexol_conversion_table()
  if (!iodine_mg_ml %in% conv_table$iodine_mg_ml) return(NA_real_)
  conv_table$iohexol_mg_ml[conv_table$iodine_mg_ml == iodine_mg_ml]
}

#' Jacobsson EECV calculation
#' @param sex Character, "Man" or "Kvinna"
#' @param weight_kg Numeric, weight in kilograms
#' @return Numeric, EECV in mL
jacobsson_eecv <- function(sex, weight_kg) {
  if (!is_valid_num(weight_kg) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  if (sex == "Man") {
    166 * weight_kg + 2490
  } else {
    95 * weight_kg + 6170
  }
}

#' One-point iohexol GFR calculation
#' @param sex Character, "Man" or "Kvinna"
#' @param weight_kg Numeric, weight in kilograms
#' @param height_cm Numeric, height in centimeters
#' @param dose_mg Numeric, iohexol dose in milligrams
#' @param plasma_mg_L Numeric, plasma concentration in mg/L
#' @param t_min Numeric, time after injection in minutes
#' @param bsa_formula Character, BSA formula key (e.g., "dubois")
#' @return List, containing non-adjusted GFR (mL/min), adjusted GFR (mL/min/1.73m²), optimal sampling time (hours), or NA if invalid
iohexol_one_point_gfr <- function(sex, weight_kg, height_cm, dose_mg, plasma_mg_L, t_min, bsa_formula) {
  if (!is_valid_num(weight_kg) || !is_valid_num(height_cm) || !is_valid_num(dose_mg) ||
      !is_valid_num(plasma_mg_L) || !is_valid_num(t_min) || !sex %in% c("Man", "Kvinna") ||
      t_min <= 0 || plasma_mg_L <= 0 || !bsa_formula %in% BSA_FORMULAS) {
    return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  }
  
  # Step 1: Calculate BSA
  bsa <- bsa_select(weight_kg, height_cm, sex, bsa_formula)
  if (!is_valid_num(bsa)) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  
  # Step 2: Calculate BSA factor
  bsa_factor <- 1.73 / bsa
  if (!is_valid_num(bsa_factor)) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  
  # Step 3: Calculate EECV
  eecv <- jacobsson_eecv(sex, weight_kg)
  if (!is_valid_num(eecv)) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  
  # Step 4: Calculate Clv
  arg_clv <- (1000 * dose_mg) / (eecv * plasma_mg_L)
  if (!is_valid_num(arg_clv) || arg_clv <= 1) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  clv <- 1 / (t_min / eecv + 0.0016) * log(arg_clv)
  if (!is_valid_num(clv)) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  
  # Step 5: Calculate m
  m <- 0.991 - 0.00122 * clv
  if (!is_valid_num(m) || m <= 0 || m > 1) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  
  # Step 6: Calculate v'
  v_prime <- eecv / m
  if (!is_valid_num(v_prime)) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  
  # Step 7: Calculate non-adjusted mGFR
  arg_gfr <- (1000 * dose_mg) / (v_prime * plasma_mg_L)
  if (!is_valid_num(arg_gfr) || arg_gfr <= 1) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  gfr <- 1 / (t_min / v_prime + 0.0016) * log(arg_gfr)
  if (!is_valid_num(gfr)) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  
  # Step 8: Calculate optimal sampling time (v' / mGFR, in hours)
  t_opt_hours <- (v_prime / gfr) / 60
  if (!is_valid_num(t_opt_hours)) t_opt_hours <- NA_real_
  
  # Step 9: Calculate adjusted mGFR
  gfr_adj <- gfr * bsa_factor
  if (!is_valid_num(gfr_adj)) return(list(gfr = NA_real_, gfr_adj = NA_real_, t_opt_hours = NA_real_))
  
  return(list(gfr = gfr, gfr_adj = gfr_adj, t_opt_hours = t_opt_hours))
}

#' Slope-intercept GFR (multi-point: two or four)
#' @param times_min Numeric vector, times in minutes post-injection
#' @param conc_mg_L Numeric vector, concentrations in mg/L
#' @param dose_mg Numeric, dose in mg
#' @param bsa Numeric, body surface area in m²
#' @return List, containing non-adjusted GFR (mL/min), adjusted GFR (mL/min/1.73m²), or NA if invalid
slope_intercept_gfr <- function(times_min, conc_mg_L, dose_mg, bsa) {
  if (length(times_min) != length(conc_mg_L) || length(times_min) < 2 ||
      any(!is_valid_num(c(times_min, conc_mg_L))) || !is_valid_num(dose_mg) || !is_valid_num(bsa)) {
    return(list(gfr = NA_real_, gfr_adj = NA_real_))
  }
  
  # Log-linear fit (natural log)
  df <- data.frame(t = as.numeric(times_min), conc = as.numeric(conc_mg_L))
  fit <- tryCatch(lm(log(conc) ~ t, data = df), error = function(e) NULL)
  
  if (is.null(fit) || any(is.na(coef(fit)))) {
    return(list(gfr = NA_real_, gfr_adj = NA_real_))
  }
  
  intercept_log <- coef(fit)[1] # Intercept on log scale (ln C at t=0)
  slope <- coef(fit)[2]         # Slope on log scale
  
  # Slope must be negative (concentration decreases over time)
  if (slope >= 0) {
    return(list(gfr = NA_real_, gfr_adj = NA_real_))
  }
  
  k_pos <- -slope # Elimination constant k (/min)
  
  # Calculate AUC = C0/k = (exp(intercept_log)) / k
  # Clearance (Cl1) = Dose / AUC
  # Units: Cl1 (L/min) = dose (mg) / ( (mg/L) / (/min) ) = dose / ( (mg/L) * min )
  auc <- exp(intercept_log) / k_pos
  cl1_L_min <- dose_mg / auc
  
  # Convert L/min to mL/min for Brøchner-Mortensen correction
  cl1_ml_min <- cl1_L_min * 1000
  
  # Brøchner-Mortensen correction
  gfr_bm <- 0.99078 * cl1_ml_min - 0.001218 * (cl1_ml_min^2)
  if (!is_valid_num(gfr_bm)) return(list(gfr = NA_real_, gfr_adj = NA_real_))
  
  # BSA adjustment
  gfr_adj <- gfr_bm * (1.73 / bsa)
  if (!is_valid_num(gfr_adj)) return(list(gfr = NA_real_, gfr_adj = NA_real_))
  
  return(list(gfr = gfr_bm, gfr_adj = gfr_adj))
}