#' EKFC eGFR formula
#' @param value Numeric, biomarker value
#' @param Q Numeric, reference biomarker value
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
ekfc_gfr <- function(value, Q, age) {
  if (!is_valid_num(value) || !is_valid_num(Q)) return(NA_real_)
  ratio <- value / Q
  gfr <- ifelse(ratio < 1, 107.3 * ratio^(-0.322), 107.3 * ratio^(-1.132))
  if (is_valid_num(age, 40)) gfr <- gfr * 0.990^(age - 40)
  return(gfr)
}

#' Q value for cystatin C, sex-adjusted
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, Q value
q_cysc_sexadj <- function(sex, age) {
  if (!is_valid_num(age) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  if (sex == "Man") {
    if (age < 50) 0.86 else 0.86 + 0.005 * (age - 50)
  } else {
    if (age < 50) 0.79 else 0.79 + 0.005 * (age - 50)
  }
}

#' FAS eGFR using creatinine
#' @param scr Numeric, serum creatinine in mg/dL
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
fas_gfr_cr <- function(scr, sex, age) {
  if (!is_valid_num(scr) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  QCr <- ifelse(sex == "Man", 0.9, 0.7)
  ratio <- scr / QCr
  egfr <- 107.3 * (ratio^(-0.87))
  if (is_valid_num(age, 20)) egfr <- egfr * 0.988^(age - 20)
  return(egfr)
}

#' FAS eGFR using cystatin C
#' @param scys Numeric, serum cystatin C in mg/L
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
fas_gfr_cys <- function(scys, age) {
  if (!is_valid_num(scys)) return(NA_real_)
  QCys <- 0.82
  ratio <- scys / QCys
  egfr <- 107.3 * (ratio^(-0.87))
  if (is_valid_num(age, 20)) egfr <- egfr * 0.988^(age - 20)
  return(egfr)
}

#' FAS eGFR using cystatin C, sex-adjusted
#' @param scys Numeric, serum cystatin C in mg/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
fas_gfr_cys_sexadj <- function(scys, sex, age) {
  if (!is_valid_num(scys) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  Qadj <- q_cysc_sexadj(sex, age)
  ratio <- scys / Qadj
  egfr <- 107.3 * (ratio^(-0.87))
  if (is_valid_num(age, 20)) egfr <- egfr * 0.988^(age - 20)
  return(egfr)
}

#' Revised LMR eGFR using creatinine
#' @param scr_umol Numeric, serum creatinine in µmol/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
r_lmr_gfr_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  QCr <- ifelse(sex == "Man", 80, 62)
  ratio <- scr_umol / QCr
  egfr <- 99.0 * (ratio^(-0.556))
  if (is_valid_num(age, 16)) egfr <- egfr * 0.993^(age - 16)
  return(egfr)
}

#' Revised LMR eGFR using cystatin C
#' @param scys Numeric, serum cystatin C in mg/L
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
r_lmr_gfr_cys <- function(scys, age) {
  if (!is_valid_num(scys)) return(NA_real_)
  QCys <- 0.83
  ratio <- scys / QCys
  egfr <- 99.0 * (ratio^(-0.556))
  if (is_valid_num(age, 16)) egfr <- egfr * 0.993^(age - 16)
  return(egfr)
}

#' Revised LMR eGFR using cystatin C, sex-adjusted
#' @param scys Numeric, serum cystatin C in mg/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
r_lmr_gfr_cys_sexadj <- function(scys, sex, age) {
  if (!is_valid_num(scys) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  Qadj <- q_cysc_sexadj(sex, age)
  ratio <- scys / Qadj
  egfr <- 99.0 * (ratio^(-0.556))
  if (is_valid_num(age, 16)) egfr <- egfr * 0.993^(age - 16)
  return(egfr)
}

#' CKD-EPI 2021 eGFR using creatinine
#' @param scr_umol Numeric, serum creatinine in µmol/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
ckdepi2021_gfr_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  scr <- scr_umol / 88.4
  kappa <- ifelse(sex == "Man", 0.9, 0.7)
  alpha <- ifelse(sex == "Man", -0.302, -0.241)
  min_scr <- min(scr / kappa, 1)
  max_scr <- max(scr / kappa, 1)
  sexfac <- ifelse(sex == "Kvinna", 1.012, 1)
  egfr <- 142 * (min_scr ^ alpha) * (max_scr ^ -1.200) * (0.9938^age) * sexfac
  return(egfr)
}

#' CKD-EPI 2021 eGFR using cystatin C
#' @param scys Numeric, serum cystatin C in mg/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
ckdepi2021_gfr_cys <- function(scys, sex, age) {
  if (!is_valid_num(scys) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  min_cys <- min(scys / 0.8, 1)
  max_cys <- max(scys / 0.8, 1)
  sexfac <- ifelse(sex == "Kvinna", 0.932, 1)
  egfr <- 133 * (min_cys ^ -0.499) * (max_cys ^ -1.328) * (0.996^age) * sexfac
  return(egfr)
}

#' CKD-EPI 2021 eGFR using creatinine and cystatin C
#' @param scr_umol Numeric, serum creatinine in µmol/L
#' @param scys Numeric, serum cystatin C in mg/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
ckdepi2021_gfr_cr_cys <- function(scr_umol, scys, sex, age) {
  if (!is_valid_num(scr_umol) || !is_valid_num(scys) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  scr <- scr_umol / 88.4
  kappa <- ifelse(sex == "Man", 0.9, 0.7)
  alpha <- ifelse(sex == "Man", -0.144, -0.219)
  min_scr <- min(scr / kappa, 1)
  max_scr <- max(scr / kappa, 1)
  min_cys <- min(scys / 0.8, 1)
  max_cys <- max(scys / 0.8, 1)
  sexfac <- ifelse(sex == "Kvinna", 0.963, 1)
  egfr <- 135 * (min_scr ^ alpha) * (max_scr ^ -0.544) *
    (min_cys ^ -0.323) * (max_cys ^ -0.778) *
    (0.9961^age) * sexfac
  return(egfr)
}

#' LMR18 eGFR using creatinine
#' @param scr_umol Numeric, serum creatinine in µmol/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
lmr18_gfr_cr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  if (sex == "Man") {
    if (scr_umol < 180) {
      X <- 2.56 + 0.00968 * (180 - scr_umol)
    } else {
      X <- 2.56 - 0.926 * log(scr_umol / 180)
    }
  } else {
    if (scr_umol < 150) {
      X <- 2.50 + 0.0121 * (150 - scr_umol)
    } else {
      X <- 2.50 - 0.926 * log(scr_umol / 150)
    }
  }
  egfr <- exp(X - 0.0158 * age + 0.438 * log(age))
  return(egfr)
}

#' CAPA eGFR using cystatin C
#' @param scys Numeric, serum cystatin C in mg/L
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
capa_gfr_cys <- function(scys, age) {
  if (!is_valid_num(scys) || !is_valid_num(age)) return(NA_real_)
  egfr <- 130 * scys^(-1.069) * age^(-0.117) - 7
  return(egfr)
}

#' MDRD eGFR
#' @param scr_umol Numeric, serum creatinine in µmol/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
mdrd_gfr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  scr_mgdL <- scr_umol / 88.4
  sexfac <- ifelse(sex == "Kvinna", 0.742, 1)
  egfr <- 175 * (scr_mgdL^(-1.154)) * (age^(-0.203)) * sexfac
  return(egfr)
}

#' BIS eGFR
#' @param scr_umol Numeric, serum creatinine in µmol/L
#' @param sex Character, "Man" or "Kvinna"
#' @param age Numeric, age in years
#' @return Numeric, eGFR in mL/min/1.73m^2
bis_gfr <- function(scr_umol, sex, age) {
  if (!is_valid_num(scr_umol) || !sex %in% c("Man", "Kvinna")) return(NA_real_)
  scr_mgdL <- scr_umol / 88.4
  sexfac <- ifelse(sex == "Kvinna", 0.82, 1)
  egfr <- 3736 * (scr_mgdL^(-0.87)) * (age^(-0.95)) * sexfac
  return(egfr)
}