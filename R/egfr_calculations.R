# --- eGFR (Adult & Child) Formula Implementations ---

#' Calculate Q-value for Cystatin C
#'
#' This function centralizes the Q-value calculation for adult eGFR formulas.
#' @param age Numeric, age in years.
#' @param sex Character, "Man" or "Kvinna". If NULL, only age-adjustment is applied.
#' @return Numeric, the calculated Q-value for Cystatin C.
calculate_q_cysc <- function(age, sex = NULL) {
  if (!is_valid_num(age, min = 18)) return(NA)
  
  # Base Q-value calculation (age-adjusted for r-LMR)
  q_val <- if (age < 50) {
    0.83
  } else {
    0.83 + 0.005 * (age - 50)
  }
  
  # Apply sex-adjustment if sex is provided (for EKFC* and r-LMR*)
  if (!is.null(sex)) {
    q_val <- 0.83 * (0.996^age) # Base for sex-adjusted is different
    if (sex == "Man") {
      q_val <- q_val * 1.05
    }
  }
  
  return(q_val)
}


# Piecewise EKFC GFR calculation
ekfc_gfr <- function(marker_value, q_value, alpha_lt1, alpha_ge1) {
  if (!is_valid_num(marker_value) || !is_valid_num(q_value)) return(NA)
  ratio <- marker_value / q_value
  exponent <- ifelse(ratio < 1, alpha_lt1, alpha_ge1)
  return(107.3 * (ratio ^ exponent))
}

# EKFC (Child) Creatinine Q-Value (µmol/L based)
q_cr_child <- function(sex, age) {
  if (!is_valid_num(age, min = 2)) return(NA)
  if (sex == "Man") {
    log_q_umol <- 3.2 + 0.259 * age - 0.543 * log(age) - 0.00763 * age^2 + 0.000079 * age^3
  } else {
    log_q_umol <- 3.08 + 0.177 * age - 0.223 * log(age) - 0.00596 * age^2 + 0.0000686 * age^3
  }
  return(exp(log_q_umol))
}

fas_gfr_cr <- function(scr, age) {
  if (!is_valid_num(scr) || !is_valid_num(age)) return(NA)
  gfr <- 107.3 / (scr / 0.7)
  if (age > 40) gfr <- gfr * (0.988^(age - 40))
  return(gfr)
}

# Correct FAS for cystatin C uses a single Q of 0.8
fas_gfr_cys <- function(scys, age) {
  if (!is_valid_num(scys) || !is_valid_num(age)) return(NA)
  gfr <- 107.3 / (scys / 0.8)
  if (age > 40) gfr <- gfr * (0.988^(age - 40))
  return(gfr)
}

# Corrected r-LMR for Creatinine
r_lmr_gfr_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !is_valid_num(age)) return(NA)
  x_val <- if (sex == "Man") {
    ifelse(scr_umol < 180, 2.56 + 0.00968 * (180 - scr_umol), 2.56 - 0.926 * log(scr_umol / 180))
  } else { # Kvinna
    ifelse(scr_umol < 150, 2.50 + 0.0121 * (150 - scr_umol), 2.50 - 0.926 * log(scr_umol / 150))
  }
  return(exp(x_val - 0.0158 * age + 0.438 * log(age)))
}

# Corrected r-LMR for Cystatin C
r_lmr_gfr_cys <- function(scys, age, q_value) {
  if (!is_valid_num(scys) || !is_valid_num(age) || !is_valid_num(q_value)) return(NA)
  
  b_over_q <- scys / q_value
  
  x_val <- if (b_over_q < 2.33) {
    4.3087 - 0.7623 * b_over_q
  } else {
    3.3145 - 0.9260 * log(b_over_q)
  }
  
  return(exp(x_val - 0.0158 * age + 0.438 * log(age)))
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
  if (!is_valid_num(c(scr_umol, scys, age))) return(NA)
  scr_mgdl <- scr_umol / 88.4
  kappa_cr <- ifelse(sex == "Man", 0.9, 0.7)
  alpha_cr <- ifelse(sex == "Man", -0.219, -0.248)
  sex_factor <- ifelse(sex == "Man", 1, 1.018)
  135 * (min(scr_mgdl/kappa_cr,1)^alpha_cr) * (max(scr_mgdl/kappa_cr,1)^-0.601) *
    (min(scys/0.8,1)^-0.375) * (max(scys/0.8,1)^-0.711) * (0.9961^age) * sex_factor
}

lmr18_gfr_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(c(scr_umol, age))) return(NA)
  x_val <- if (sex == "Man") {
    ifelse(scr_umol < 180, 2.56 + 0.00968 * (180 - scr_umol), 2.56 - 0.926 * log(scr_umol / 180))
  } else {
    ifelse(scr_umol < 150, 2.50 + 0.0121 * (150 - scr_umol), 2.50 - 0.926 * log(scr_umol / 150))
  }
  return(exp(x_val - 0.0158 * age + 0.438 * log(age)))
}

capa_gfr_cys <- function(scys, age) {
  if (!is_valid_num(c(scys, age))) return(NA)
  130 * (scys^-1.069) * (age^-0.117) - 7
}

mdrd_gfr <- function(scr_umol, sex, age) {
  if (!is_valid_num(c(scr_umol, age))) return(NA)
  sex_factor <- ifelse(sex == "Man", 1, 0.742)
  175 * ((scr_umol / 88.4)^-1.154) * (age^-0.203) * sex_factor
}

# Corrected BIS formula (from image)
bis_gfr <- function(scr_umol, sex, age) {
  if (!is_valid_num(c(scr_umol, age))) return(NA)
  
  # Convert scr from µmol/L to mg/dL for the formula
  scr_mgdl <- scr_umol / 88.4
  
  # Determine sex factor (1 for male, 0.82 for female)
  sex_factor <- ifelse(sex == "Kvinna", 0.82, 1)
  
  # Calculate eGFR using the correct formula
  gfr <- 3736 * (scr_mgdl^-0.87) * (age^-0.95) * sex_factor
  
  return(gfr)
}


schwartz_gfr <- function(height_cm, scr_mgdl) {
  if (!is_valid_num(c(height_cm, scr_mgdl))) return(NA)
  0.413 * (height_cm / scr_mgdl)
}

ckid_u25_scr <- function(sex, age, height_m, scr_mgdl) {
  if (!is_valid_num(c(age, height_m, scr_mgdl))) return(NA)
  k_htdscr <- if (sex == "Man") {
    if (age < 12) 39.0 * 1.008^(age - 12) else if (age < 18) 39.0 * 1.045^(age - 12) else 50.8
  } else {
    if (age < 12) 36.1 * 1.008^(age - 12) else if (age < 18) 36.1 * 1.023^(age - 12) else 41.4
  }
  return(k_htdscr * (height_m / scr_mgdl))
}

ckid_u25_cys <- function(sex, age, scys) {
  if (!is_valid_num(c(age, scys))) return(NA)
  k_cysc <- if (sex == "Man") {
    if (age < 15) 87.2 * 1.011^(age - 15) else if (age < 18) 87.2 * 0.960^(age - 15) else 77.1
  } else {
    if (age < 12) 79.9 * 1.004^(age - 12) else if (age < 18) 79.9 * 0.974^(age - 12) else 68.3
  }
  return(k_cysc * (1 / scys))
}

lmr18_gfr_child_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(c(scr_umol, age)) || age < 2 || age >= 18) return(NA)
  log_scr <- log(scr_umol)
  log_adj_scr <- if (sex == "Man") {
    log_scr + 0.259*(18-age) - 0.543*log(18/age) - 0.00763*(18^2-age^2) + 0.0000790*(18^3-age^3)
  } else {
    log_scr + 0.177*(18-age) - 0.223*log(18/age) - 0.00596*(18^2-age^2) + 0.0000686*(18^3-age^3)
  }
  return(lmr18_gfr_cr(exp(log_adj_scr), sex, 18))
}

zappitelli_gfr_cys <- function(scys) {
  if (!is_valid_num(scys)) return(NA)
  75.94 / (scys^1.17)
}

zappitelli_gfr_cys_cr <- function(scr_umol, scys, height_cm) {
  if (!is_valid_num(c(scr_umol, scys, height_cm))) return(NA)
  (507.76 * exp(0.003 * height_cm)) / (scys^0.635 * scr_umol^0.547)
}

calc_cockcroft_gault <- function(age, weight, scr_umol, sex) {
  if (!is_valid_num(c(age, weight, scr_umol))) return(NA)
  scr_mgdl <- scr_umol / 88.4
  clearance <- ((140 - age) * weight) / (72 * scr_mgdl)
  if (sex == "Kvinna") clearance <- clearance * 0.85
  return(clearance)
}