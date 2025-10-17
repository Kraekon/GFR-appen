library(ggplot2)
library(dplyr)
library(DT)

# --- Utility Functions ---

# Check if a number is valid (not NULL, NA, or non-finite)
is_valid_num <- function(x, min = -Inf, max = Inf, inclusive = TRUE) {
  if (is.null(x) || !is.numeric(x) || !is.finite(x) || is.na(x)) {
    return(FALSE)
  }
  if (inclusive) {
    return(x >= min && x <= max)
  } else {
    return(x > min && x < max)
  }
}

# --- BSA Formula Implementations ---

calc_dubois <- function(weight, height) { 0.007184 * (weight^0.425) * (height^0.725) }
calc_dubois_tweaked <- function(weight, height) { 0.007184 * (weight^0.428) * (height^0.725) }
calc_haycock <- function(weight, height) { 0.024265 * (weight^0.5378) * (height^0.3964) }
calc_boyd <- function(weight, height) { 0.0003207 * (weight^(0.7285 - 0.0188 * log10(weight))) * (height^0.3) }
calc_gehan <- function(weight, height) { 0.0235 * (weight^0.51456) * (height^0.42246) }
calc_fujimoto <- function(weight, height) { 0.008883 * (weight^0.444) * (height^0.663) }
calc_schlich <- function(weight, sex) {
  if (sex == "Man") {
    return(0.000975482 * (weight^0.46) * (height^1.08))
  } else {
    return(0.000578848 * (weight^0.38) * (height^1.24))
  }
}

bsa_select <- function(weight, height, sex, formula_name) {
  if (!is_valid_num(weight) || !is_valid_num(height)) return(NA)
  switch(formula_name,
         "dubois" = calc_dubois(weight, height),
         "dubois_tweaked" = calc_dubois_tweaked(weight, height),
         "haycock" = calc_haycock(weight, height),
         "boyd" = calc_boyd(weight, height),
         "gehan" = calc_gehan(weight, height),
         "fujimoto" = calc_fujimoto(weight, height),
         "schlich" = calc_schlich(weight, sex),
         NA)
}

# --- Iohexol Calculation Functions ---

# Convert Iodine concentration to Iohexol concentration
iodine_to_iohexol <- function(iodine_mg_ml) {
  if (!is_valid_num(iodine_mg_ml)) return(NA)
  return(iodine_mg_ml / 0.491)
}

# Calculate time difference in minutes from HH:MM strings
calc_time_difference_min <- function(time1_str, time2_str) {
  time1 <- as.POSIXct(time1_str, format = "%H:%M")
  time2 <- as.POSIXct(time2_str, format = "%H:%M")
  if (is.na(time1) || is.na(time2)) return(NA)
  return(as.numeric(difftime(time2, time1, units = "mins")))
}

# Jacobsson's formula for EECV
jacobsson_eecv <- function(sex, weight) {
  if (sex == "Man") {
    eecv <- 1.88 * weight + 28.3
  } else {
    eecv <- 4.17 * weight + 8.1
  }
  return(eecv)
}

# 1-point GFR calculation
iohexol_one_point_gfr <- function(sex, weight, height, dose_mg, plasma_mg_L, t_diff_min, bsa_formula) {
  if (!is_valid_num(dose_mg) || !is_valid_num(plasma_mg_L) || !is_valid_num(t_diff_min)) return(list(gfr=NA, gfr_adj=NA, t_opt_hours=NA))
  
  eecv <- jacobsson_eecv(sex, weight)
  c0 <- dose_mg / (eecv / 1000)
  
  slope <- (log(plasma_mg_L) - log(c0)) / t_diff_min
  gfr <- -slope * (eecv / 1000) * 1000
  
  bsa <- bsa_select(weight, height, sex, bsa_formula)
  gfr_adj <- if (is_valid_num(bsa)) gfr * (1.73 / bsa) else NA
  
  t_opt_hours <- if (is_valid_num(gfr)) 25300 / gfr / 60 else NA
  
  return(list(gfr = gfr, gfr_adj = gfr_adj, t_opt_hours = t_opt_hours))
}

# Slope-intercept GFR calculation for 2 or 4 points
slope_intercept_gfr <- function(times_min, concs_num, dose_mg, bsa) {
  if (length(times_min) != length(concs_num) || length(times_min) < 2) return(list(gfr=NA, gfr_adj=NA))
  
  df <- data.frame(time = times_min, conc = concs_num)
  fit <- lm(log(conc) ~ time, data = df)
  
  intercept <- coef(fit)[1]
  slope <- coef(fit)[2]
  
  c0 <- exp(intercept)
  gfr <- (dose_mg / c0) * (-slope) * 1000
  
  gfr_adj <- if (is_valid_num(bsa)) gfr * (1.73 / bsa) else NA
  
  return(list(gfr = gfr, gfr_adj = gfr_adj))
}

# --- eGFR (Adult & Child) Formula Implementations ---

# Corrected, piecewise EKFC GFR calculation
ekfc_gfr <- function(marker_value, q_value, alpha_lt1, alpha_ge1) {
  if (!is_valid_num(marker_value) || !is_valid_num(q_value)) return(NA)
  
  ratio <- marker_value / q_value
  
  if (ratio < 1) {
    exponent <- alpha_lt1
  } else {
    exponent <- alpha_ge1
  }
  
  return(107.3 * (ratio ^ exponent))
}

# EKFC (Adult) Cystatin C Q-Value (with sex adjustment)
q_cysc_sexadj <- function(sex, age) {
  if (!is_valid_num(age)) return(NA)
  q <- 0.83 * (0.996^age) # The base Q for CysC is 0.83, age adjusted
  if (sex == "Man") q <- q * 1.05
  return(q)
}

# EKFC (Child) Creatinine Q-Value (µmol/L based)
q_cr_child <- function(sex, age) {
  if (!is_valid_num(age, min = 2)) return(NA)
  
  if (sex == "Man") { # Pojke
    log_q_umol <- 3.2 + 0.259 * age - 0.543 * log(age) - 0.00763 * age^2 + 0.000079 * age^3
  } else { # Flicka
    log_q_umol <- 3.08 + 0.177 * age - 0.223 * log(age) - 0.00596 * age^2 + 0.0000686 * age^3
  }
  return(exp(log_q_umol))
}

fas_gfr_cr <- function(scr, sex, age) {
  if (!is_valid_num(scr) || !is_valid_num(age)) return(NA)
  q_cr <- ifelse(sex == "Man", 32, 25)
  gfr <- 107.3 / (scr / 0.7)
  if (age > 40) {
    gfr <- gfr * (0.988^(age - 40))
  }
  return(gfr)
}

fas_gfr_cys <- function(scys, age) {
  if (!is_valid_num(scys) || !is_valid_num(age)) return(NA)
  gfr <- 107.3 / (scys / 0.8)
  if (age > 40) {
    gfr <- gfr * (0.988^(age - 40))
  }
  return(gfr)
}

fas_gfr_cys_sexadj <- function(scys, sex, age) {
  if (!is_valid_num(scys) || !is_valid_num(age)) return(NA)
  q_cys <- ifelse(sex == "Man", 0.82 * 1.05, 0.82)
  gfr <- 107.3 / (scys / q_cys)
  return(gfr)
}

# Corrected LMR18 for Adults (Creatinine)
lmr18_gfr_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !is_valid_num(age)) return(NA)
  
  x_val <- 0
  if (sex == "Man") {
    if (scr_umol < 180) {
      x_val <- 2.56 + 0.00968 * (180 - scr_umol)
    } else {
      x_val <- 2.56 - 0.926 * log(scr_umol / 180)
    }
  } else { # Kvinna
    if (scr_umol < 150) {
      x_val <- 2.50 + 0.0121 * (150 - scr_umol)
    } else {
      x_val <- 2.50 - 0.926 * log(scr_umol / 150)
    }
  }
  
  return(exp(x_val - 0.0158 * age + 0.438 * log(age)))
}


r_lmr_gfr_cys <- function(scys, age) {
  if (!is_valid_num(scys) || !is_valid_num(age)) return(NA)
  exp(4.5 - 0.017 * age - 0.93 * log(scys))
}

r_lmr_gfr_cys_sexadj <- function(scys, sex, age) {
  if (!is_valid_num(scys) || !is_valid_num(age)) return(NA)
  sex_factor <- ifelse(sex == "Man", 1, 1.09)
  exp(4.4 - 0.017 * age + 0.1 * sex_factor - 0.93 * log(scys))
}

ckdepi2021_gfr_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !is_valid_num(age)) return(NA)
  scr_mgdl <- scr_umol / 88.4
  kappa <- ifelse(sex == "Man", 0.9, 0.7)
  alpha <- ifelse(sex == "Man", -0.302, -0.241)
  sex_factor <- ifelse(sex == "Man", 1, 1.012)
  142 * (min(scr_mgdl / kappa, 1)^alpha) * (max(scr_mgdl / kappa, 1)^-1.2) * (0.9938^age) * sex_factor
}

ckdepi2021_gfr_cys <- function(scys, sex, age) {
  if (!is_valid_num(scys) || !is_valid_num(age)) return(NA)
  kappa <- 0.8
  alpha <- -0.4
  sex_factor <- ifelse(sex == "Man", 1, 1.012)
  130 * (min(scys / kappa, 1)^alpha) * (max(scys / kappa, 1)^-0.6) * (0.996^age) * sex_factor
}

ckdepi2021_gfr_cr_cys <- function(scr_umol, scys, sex, age) {
  if (!is_valid_num(scr_umol) || !is_valid_num(scys) || !is_valid_num(age)) return(NA)
  scr_mgdl <- scr_umol / 88.4
  kappa_cr <- ifelse(sex == "Man", 0.9, 0.7)
  alpha_cr <- ifelse(sex == "Man", -0.219, -0.248)
  sex_factor <- ifelse(sex == "Man", 1, 1.018)
  135 * (min(scr_mgdl / kappa_cr, 1)^alpha_cr) * (max(scr_mgdl / kappa_cr, 1)^-0.601) * (min(scys / 0.8, 1)^-0.375) * (max(scys / 0.8, 1)^-0.711) * (0.9961^age) * sex_factor
}

# Corrected CAPA formula
capa_gfr_cys <- function(scys, age) {
  if (!is_valid_num(scys) || !is_valid_num(age)) return(NA)
  130 * (scys^-1.069) * (age^-0.117) - 7
}

mdrd_gfr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !is_valid_num(age)) return(NA)
  sex_factor <- ifelse(sex == "Man", 1, 0.742)
  175 * ((scr_umol / 88.4)^-1.154) * (age^-0.203) * sex_factor
}

bis_gfr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !is_valid_num(age)) return(NA)
  sex_factor <- ifelse(sex == "Man", 0, -0.228)
  3736 * (scr_umol^-0.87) * (age^-0.95) * exp(sex_factor)
}

# --- eGFR (Child) Formula Implementations ---

schwartz_gfr <- function(height_cm, scr_mgdl) {
  if (!is_valid_num(height_cm) || !is_valid_num(scr_mgdl)) return(NA)
  0.413 * (height_cm / scr_mgdl)
}

# CKiD U25 Creatinine
ckid_u25_scr <- function(sex, age, height_m, scr_mgdl) {
  if (!is_valid_num(age) || !is_valid_num(height_m) || !is_valid_num(scr_mgdl)) return(NA)
  
  k_htdscr <- 0
  if (sex == "Man") {
    if (age < 12) k_htdscr <- 39.0 * 1.008^(age - 12)
    else if (age >= 12 && age < 18) k_htdscr <- 39.0 * 1.045^(age - 12)
    else k_htdscr <- 50.8
  } else { # Kvinna
    if (age < 12) k_htdscr <- 36.1 * 1.008^(age - 12)
    else if (age >= 12 && age < 18) k_htdscr <- 36.1 * 1.023^(age - 12)
    else k_htdscr <- 41.4
  }
  
  return(k_htdscr * (height_m / scr_mgdl))
}

# CKiD U25 Cystatin C
ckid_u25_cys <- function(sex, age, scys) {
  if (!is_valid_num(age) || !is_valid_num(scys)) return(NA)
  
  k_cysc <- 0
  if (sex == "Man") {
    if (age < 15) k_cysc <- 87.2 * 1.011^(age - 15)
    else if (age >= 15 && age < 18) k_cysc <- 87.2 * 0.960^(age - 15)
    else k_cysc <- 77.1
  } else { # Kvinna
    if (age < 12) k_cysc <- 79.9 * 1.004^(age - 12)
    else if (age >= 12 && age < 18) k_cysc <- 79.9 * 0.974^(age - 12)
    else k_cysc <- 68.3
  }
  
  return(k_cysc * (1 / scys))
}

# LMR18 for Children (creatinine)
lmr18_gfr_child_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !is_valid_num(age, min = 2, max = 18, inclusive = FALSE)) return(NA)
  
  log_scr <- log(scr_umol)
  
  if (sex == "Man") { # Pojke
    log_adj_scr <- log_scr + 0.259 * (18 - age) - 0.543 * log(18 / age) - 0.00763 * (18^2 - age^2) + 0.0000790 * (18^3 - age^3)
  } else { # Flicka
    log_adj_scr <- log_scr + 0.177 * (18 - age) - 0.223 * log(18 / age) - 0.00596 * (18^2 - age^2) + 0.0000686 * (18^3 - age^3)
  }
  
  adj_scr_umol <- exp(log_adj_scr)
  
  # Use the adult LMR18 formula with the adjusted creatinine and age 18
  return(lmr18_gfr_cr(adj_scr_umol, sex, 18))
}

# Zappitelli (Cystatin C only)
zappitelli_gfr_cys <- function(scys) {
  if (!is_valid_num(scys)) return(NA)
  75.94 / (scys^1.17)
}

# Zappitelli (Creatinine and Cystatin C)
zappitelli_gfr_cys_cr <- function(scr_umol, scys, height_cm) {
  if (!is_valid_num(scr_umol) || !is_valid_num(scys) || !is_valid_num(height_cm)) return(NA)
  (507.76 * exp(0.003 * height_cm)) / (scys^0.635 * scr_umol^0.547)
}