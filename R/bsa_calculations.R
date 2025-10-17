# --- BSA Formula Implementations ---

calc_dubois <- function(weight, height) { 0.007184 * (weight^0.425) * (height^0.725) }
calc_dubois_tweaked <- function(weight, height) { 0.007184 * (weight^0.428) * (height^0.725) }
calc_haycock <- function(weight, height) { 0.024265 * (weight^0.5378) * (height^0.3964) }

#' Boyd BSA formula (Corrected)
#' The original formula uses weight in grams. This implementation converts from kg.
#' The argument names are now consistent with the other functions in this file.
calc_boyd <- function(weight, height) {
  weight_g <- weight * 1000
  exponent <- 0.7285 - (0.0188 * log10(weight_g))
  return(0.0003207 * (height^0.3) * (weight_g^exponent))
}

calc_gehan <- function(weight, height) { 0.0235 * (weight^0.51456) * (height^0.42246) }
calc_fujimoto <- function(weight, height) { 0.008883 * (weight^0.444) * (height^0.663) }
calc_schlich <- function(weight, height, sex) {
  if (sex == "Man") {
    return(0.000975482 * (weight^0.46) * (height^1.08))
  } else {
    return(0.000578848 * (weight^0.38) * (height^1.24))
  }
}

bsa_select <- function(weight, height, sex, formula_name) {
  if (!is_valid_num(weight) || !is_valid_num(height)) return(NA)
  # All functions called from here now consistently use 'weight' and 'height'
  switch(formula_name,
         "dubois" = calc_dubois(weight, height),
         "dubois_tweaked" = calc_dubois_tweaked(weight, height),
         "haycock" = calc_haycock(weight, height),
         "boyd" = calc_boyd(weight, height),
         "gehan" = calc_gehan(weight, height),
         "fujimoto" = calc_fujimoto(weight, height),
         "schlich" = calc_schlich(weight, height, sex),
         NA)
}