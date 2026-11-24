#' Pediatric End-stage Liver Disease (PELD Cr) score
#' 
#' Calculates the Pediatric End-Stage Liver Disease with Creatinine (PELD-CR)
#' score for liver transplant candidates younger than 12 years.
#' 
#' @details
#' The PELD-Cr score combines candidate age, albumin, total bilirubin, INR,
#' creatinine and growth status. Growth status is represented by the lower of 
#' the age- and sex-specific CDC height-for-age and weight-for-age z-scores.
#' 
#' Growth z-scores are calculated using the LMS method and the 2000 CDC growth
#' Charts. Age-specific L, M, and S values are linearly interpolated when 
#' age_at_measurement falls between successive reference ages.
#' 
#' age_years and age at measurement may represent different age measures.
#' age_years is the fractional calendar age at the time of most recent laboratory
#' value used for the PELD-Cr calculation. Under the OPTN specification, 
#' fractional calendar age is calculated as the number of days between relevant
#' laboratory date and the date of birth divided by 365.25.
#' 
#' age_at_measurement is the candidates age in months at the time the height and
#' weight values were measured. If height and weight were measured on different 
#' dates, the OPTN calculation uses the most recent measurement date to determine
#' the age for z-score calculation.
#' 
#' Before calculation, albumin, bilirubin and INR values below 1 are set to 1. 
#' Albumin is bounded at 1.9g/dL for its score contribution. Bilirubin 
#' contributions are bounded at 4 and 40 mg/dL, and INR contributions at 2 and 
#' 10, according to the PELD-Cr specification.
#' 
#' Creatinine is bounded between 0.2 and 1.3 mg/dL for its score contribution. A
#' creatinine value of 1.3 mg/dL is used when `dialysis = TRUE`.
#' 
#' The growth term uses the minimum of the CDC height-for-age and weight-for-age
#' z-scores. This value is bounded between -5 and -2.1 before its contribution to
#' the score is calculated.
#' 
#' The component terms are combined as 
#' \deqn{PELD = (x + 1.5287) \times 10 + 2.82}
#' 
#' where \eqn{x} is the sum of the age, albumin, bilirubin, INR, growth and 
#' creatinine terms. The resulting value is rounded to nearest integer and values
#' below 6 are set to 6.
#' 
#' The `units` argument applies only to laboratory measurements. Height must be
#' supplied in centimeters and weight in kilograms regardless of the selected
#' laboratory unit system.
#' 
#' The function is vectorized. Patient-level input vectors must have equal 
#' lengths, `dialysis` may be supplied as either a single logical value or as a
#' logical vector matching the patient-level input length. Missing inputs will
#' produce a NA value.
#' 
#' @references Hsu E, Schladt DP, Wey A, Perito ER, Israni AK. Improving the
#'   predictive ability of the pediatric end-stage liver disease score for young
#'   children awaiting liver transplant. Am J Transplant. 2021
#'   Jan;21(1):222-228. doi: 10.1111/ajt.15925. Epub 2020 May 29. PMID:
#'   32306489.
#'
#' @param age_years Numeric vector of candidate age in fractional calendar years
#'  at the time of the most recent laboratory value. Values must be greater than
#'  0 and less than 12.
#' @param age_at_measurement Numeric vector of candidate age in months at the 
#'  anthropometric measurement date used to calculate the CDC height/weight
#'  z-scores.
#' @param sex Character vector specifying birth sex. Must be one of "male" or
#'  "female". 
#' @param height Numeric vector of height or length measurements in centimeters. 
#' @param weight Numeric vector of weight measurements in kilograms.
#' @param albumin Numeric vector of serum albumin concentrations. Supply values
#'  in d/dL when `units = "US"` and g/L when `units = "SI"`.
#' @param bilirubin Numeric vector of serum bilirubin concentrations. Supply 
#'  values in mg/dL when `units = "US"` and \eqn{\mu mol/L} when `units = "SI"`.
#' @param inr Numeric vector of international normalized ratio (INR) values.
#' @param creatinine Numeric vector of serum creatinine concentrations. Supply
#'  values in mg/dL when `units = "US"` and \eqn{\mu mol/L} when `units = SI`.
#' @param dialysis Logical vector or scalar indicating whether the candidate 
#'  has received dialysis twice or higher within the last 7 days or at least 24
#'  hours of continuous veno-venous hemodialysis (CVVHD) within that period. If
#'  a scalar value i set it is recycled across all observations.
#' @param units Character scalar specifying the laboratory unit system. Must be
#'  one of `US` or `SI`. This argument does not alter the required units for 
#'  height or weight.
#' @param albumin_conversion_factor Numeric scalar used to convert albumin from
#'  g/L to g/dL when `units = SI`. Default = 0.1
#' @param bilirubin_conversion_factor Numeric scalar used to convert bilirubin 
#'  from \eqn{\mu mol/L} to mg/dL when `units = "SI"`. Default = 0.0584.
#' @param creatinine_conversion_factor Numeric scalar used to convert creatinine
#'  from \eqn{\mu mol/L} to mg/dL when `units = SI`. Default = 0.0113.
#'
#' @returns An integer vector containing the calculated PELD-Cr scores.
#' @export
#'
#' @examples
#'  peld_cr(
#'   age_years = 2,
#'   age_at_measurement = 24,
#'   sex = "female",
#'   height = 85,
#'   weight = 12,
#'   albumin = 3.2,
#'   bilirubin = 2.5,
#'   inr = 1.4,
#'   creatinine = 0.4)
#'   # 17
peld_cr <- function(age_years, albumin, bilirubin, inr,
                    creatinine, age_at_measurement = NULL,
                    sex = NULL, height = NULL,
                    weight = NULL, growth_z = NULL, dialysis = FALSE,
                    units = "US", albumin_conversion_factor = 0.1,
                    bilirubin_conversion_factor = 0.0584,
                    creatinine_conversion_factor = 0.0113) {
  
  
  # VALIDATE
  numeric_inputs <- list(
    age_years = age_years,
    albumin = albumin,
    bilirubin = bilirubin,
    inr = inr,
    creatinine = creatinine
  )
  
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1L)))){
    stop("Numeric patient-level inputs must be numeric")
  }
  
  input_lengths <- vapply(numeric_inputs,
                          length, integer(1L))
  
  if(any(input_lengths == 0L)) {
    stop("Patient-level inputs must contain at least one observation")
  }
  
  if(length(unique(input_lengths)) != 1L) {
    stop("All patient-level input vectors must have the same length")
  }
  
  n <- input_lengths[[1]]
  
  .check_logical(dialysis, "dialysis", allow_na = TRUE)
  .check_scalar_or_length(dialysis, n, "dialysis")
  .check_scalar_choice(units, c("US", "SI"), "units")
  
  if(any(albumin <= 0, na.rm = TRUE)) {
    stop("albumin must be greater than 0")}
  
  if(any(bilirubin < 0, na.rm = TRUE)) {
    stop("bilirubin must be non-negative")}
  
  if(any(inr <= 0, na.rm = TRUE)) {
    stop("inr must be greater than 0")}
  
  if(any(creatinine <= 0, na.rm = TRUE)) {
    stop("creatinine must be greater than 0")}
  
  if(any(age_years < 0 | age_years >= 12, na.rm = TRUE)) {
    stop("age_years must be greater or equal to 0 and less than 12")
  }
  
  cdc_growth_input <- list(
    age_at_measurement = age_at_measurement,
    sex = sex,
    height = height,
    weight = weight
  )
  
  cdc_supplied <- !vapply(cdc_growth_input, is.null, logical(1L))
  
  external_growth_supplied <- !is.null(growth_z)
  
  if(external_growth_supplied && any(cdc_supplied)) {
    stop("Provide either growth_z or CDC growth inputs, not both")
  }
  
  if(!external_growth_supplied && !all(cdc_supplied)){
    stop("Provide either growth_z or all of 'age_at_measurement',
         'sex', 'height' and 'weight'")
  }
  
  # CONVERT UNITS
  if(units == "SI") {
    albumin <- albumin * albumin_conversion_factor
    bilirubin <- bilirubin * bilirubin_conversion_factor
    creatinine <- creatinine * creatinine_conversion_factor
  }
  
  # REP length of dialysis to vector size
  if(length(dialysis) == 1L) {
    dialysis <- rep(dialysis, n)
  }

  # CALCULATE Z SCORES
  if(external_growth_supplied) {
    if(!.is_numeric_or_na(growth_z)){
      stop("growth_z must be numeric")}
    
    if(length(growth_z) != n) {
      stop("growth_z must match patient-level input length")}
    
    if (any(!is.finite(growth_z) & !is.na(growth_z))) {
      stop("growth_z must contain finite values")}
    
    z_min <- growth_z
  }
    
  if(!external_growth_supplied){
    
    growth_numeric <- list(
      age_at_measurement = age_at_measurement,
      height = height,
      weight = weight
    )
    
    if(!all(vapply(growth_numeric, .is_numeric_or_na, logical(1L)))){
      stop("'age_at_measurement', 'height' and 'weight' must be numeric")
    }
    
    growth_lengths <- c(
      age_at_measurement = length(age_at_measurement),
      sex = length(sex),
      height = length(height),
      weight = length(weight)
    )
    if(any(growth_lengths != n )) {
      stop("CDC growth inputs must match the patient-level input lengths")
    }
    
    if(!(is.character(sex) || (is.logical(sex) && all(is.na(sex))))){
      stop("sex must be 'male' or 'female'")}
    
    if(is.logical(sex)){
      sex <- rep(NA_character_, length(sex))}
    
    sex <- tolower(sex)
    
    if(any(!is.na(sex) & !sex %in% c("male", "female"))) {
      stop("sex must be 'male' or 'female'")}
    
    if(any(age_at_measurement < 0 | age_at_measurement >= 114, na.rm = TRUE)) {
      stop("'age_at_measurement' must be greater or equal to 0 and less than 144 months")}
    
    if(any(height <= 0, na.rm = TRUE)) {
      stop("height must be greater than 0")}
    
    if(any(weight <= 0, na.rm = TRUE)){
      stop("weight must be greater than 0") }
    
    
  z_height <- .cdc_growth_z(age_months = age_at_measurement,
                            sex = sex,
                            measurement = height,
                            reference = cdc_height)
  
  z_weight <- .cdc_growth_z(age_months = age_at_measurement,
                            sex = sex,
                            measurement = weight,
                            reference = cdc_weight)
  
  z_min <- pmin(z_height, z_weight)
  }
  
  # MISSING INPUTS
  missing <- is.na(age_years) | is.na(albumin) | is.na(bilirubin) |
    is.na(inr) | is.na(creatinine) | is.na(dialysis) | is.na(z_min)
  
  .warn_missing(missing)
  
  # OPTN bounds
  age_bounded <- pmin(pmax(age_years, 1), 5.5)
  albumin_bounded <- pmin(pmax(albumin, 1), 1.9)
  bilirubin <- pmax(bilirubin, 1)
  inr <- pmax(inr, 1)
  creatinine[which(dialysis)] <- 1.3
  creatinine_bounded <- pmin(pmax(creatinine, 0.2), 1.3)
  
  z_bounded <- pmin(pmax(z_min, -5), -2.1)
  
  # CALCULATE VECTORIZED TERMS
  age_score <- -0.1967 * age_bounded
  
  albumin_score <- -1.842 * log(albumin_bounded)
  
  bilirubin_score <- 0.7854 * log(pmin(bilirubin, 4)) +
                      0.3434 * log(pmin(pmax(bilirubin, 4), 40))
  
  inr_score <- 1.981 * log(pmin(inr, 2)) +
                0.7298 * log(pmin(pmax(inr, 2), 10))
  
  growth_score <- -0.1807 * z_bounded
  
  creatinine_score <- 1.453 * log(creatinine_bounded)
  
  # COMBINE
  x <- age_score + albumin_score + bilirubin_score +
    inr_score + growth_score + creatinine_score
  
  peld <- round((x + 1.5287) * 10 + 2.82)
  
  peld <- pmax(peld, 6)
  
  peld[missing] <- NA_real_
  
  peld
}