# BSA calculation constants
BSA_FORMULAS <- c("dubois", "dubois_tweaked", "haycock", "boyd", "gehan", "fujimoto", "schlich")

#' Du Bois BSA formula
#' @param weight_kg Numeric, weight in kilograms
#' @param height_cm Numeric, height in centimeters
#' @return Numeric, BSA in square meters
bsa_dubois <- function(weight_kg, height_cm) {
  if (!is_valid_num(weight_kg) || !is_valid_num(height_cm)) return(NA_real_)
  0.007184 * (weight_kg ^ 0.425) * (height_cm ^ 0.725)
}

#' Tweaked Du Bois BSA formula (Redlarski et al. 2024)
#' @param weight_kg Numeric, weight in kilograms
#' @param height_cm Numeric, height in centimeters
#' @return Numeric, BSA in square meters
bsa_dubois_tweaked <- function(weight_kg, height_cm) {
  if (!is_valid_num(weight_kg) || !is_valid_num(height_cm)) return(NA_real_)
  0.0104 * (weight_kg ^ 0.4157) * (height_cm ^ 0.6627)
}

#' Haycock BSA formula
#' @param weight_kg Numeric, weight in kilograms
#' @param height_cm Numeric, height in centimeters
#' @return Numeric, BSA in square meters
bsa_haycock <- function(weight_kg, height_cm) {
  if (!is_valid_num(weight_kg) || !is_valid_num(height_cm)) return(NA_real_)
  0.024265 * (weight_kg ^ 0.5378) * (height_cm ^ 0.3964)
}

#' Boyd BSA formula
#' @param weight_kg Numeric, weight in kilograms
#' @param height_cm Numeric, height in centimeters
#' @return Numeric, BSA in square meters or NA if weight > 10 kg
bsa_boyd <- function(weight_kg, height_cm) {
  if (!is_valid_num(weight_kg) || !is_valid_num(height_cm) || weight_kg > 10) return(NA_real_)
  weight_g <- weight_kg * 1000
  exp_weight <- 0.8168 - 0.0154 * log10(weight_g)
  bsa_cm2 <- 4.688 * (weight_g ^ exp_weight)
  bsa_m2 <- bsa_cm2 / 10000
  return(bsa_m2)
}

#' Gehan BSA formula
#' @param weight_kg Numeric, weight in kilograms
#' @param height_cm Numeric, height in centimeters
#' @return Numeric, BSA in square meters
bsa_gehan <- function(weight_kg, height_cm) {
  if (!is_valid_num(weight_kg) || !is_valid_num(height_cm)) return(NA_real_)
  0.0235 * (height_cm ^ 0.42246) * (weight_kg ^ 0.51456)
}

#' Fujimoto BSA formula
#' @param weight_kg Numeric, weight in kilograms
#' @param height_cm Numeric, height in centimeters
#' @return Numeric, BSA in square meters
bsa_fujimoto <- function(weight_kg, height_cm) {
  if (!is_valid_num(weight_kg) || !is_valid_num(height_cm)) return(NA_real_)
  0.008883 * (weight_kg ^ 0.444) * (height_cm ^ 0.663)
}

#' Schlich BSA formula
#' @param weight_kg Numeric, weight in kilograms
#' @param height_cm Numeric, height in centimeters
#' @param sex Character, "Man" or "Kvinna"
#' @return Numeric, BSA in square meters or NA if invalid sex
bsa_schlich <- function(weight_kg, height_cm, sex) {
  if (!is_valid_num(weight_kg) || !is_valid_num(height_cm)) return(NA_real_)
  if (sex == "Man") {
    0.000579479 * (weight_kg ^ 0.38) * (height_cm ^ 1.24)
  } else if (sex == "Kvinna") {
    0.000975482 * (weight_kg ^ 0.46) * (height_cm ^ 1.08)
  } else {
    NA_real_
  }
}

#' Select and calculate BSA based on formula key
#' @param weight Numeric, weight in kilograms
#' @param height Numeric, height in centimeters
#' @param sex Character, "Man" or "Kvinna"
#' @param key Character, formula name (e.g., "dubois")
#' @return Numeric, BSA in square meters or NA if invalid
bsa_select <- function(weight, height, sex, key) {
  if (!key %in% BSA_FORMULAS) stop("Invalid BSA formula: ", key)
  switch(key,
         "dubois" = bsa_dubois(weight, height),
         "dubois_tweaked" = bsa_dubois_tweaked(weight, height),
         "haycock" = bsa_haycock(weight, height),
         "boyd" = bsa_boyd(weight, height),
         "gehan" = bsa_gehan(weight, height),
         "fujimoto" = bsa_fujimoto(weight, height),
         "schlich" = bsa_schlich(weight, height, sex),
         NA_real_)
}