#' Title Model for End-stage Liver Disease - Sodium score
#'
#' Calculates the Model for End-Stage Liver Disease incorporating serum sodium (MELD-Na) score. 
#' The calculation follows the 2016 MELD-Na formulation
#' 
#' @details
#' The MELD-Na score is calculated from S-creatinine, S-bilirubin, INR and S-sodium.
#' Creatinine, bilirubin and INR are bounded so values < 1 are set to 1. Sodium is bounded between
#' 125 and 137 Both US and SI units are applicable for this function, where SI units are automatically converted to mg/dl before calculation.
#' The MELD-Na calculation follows two steps:
#' \enumerate{
#'    \item MELD(i) is calculated as:
#'    \deqn{
#'      MELD(i) = 10 \times round(
#'        0.957 \times \log(Creatinine) + 
#'        0.378 \times \log(Bilirubin) + 
#'        1.120 \times \log(INR) + 0.643, digits = 10)}
#'    \item If MELD(i) > 11, the sodium adjustment is applied:
#'    \deqn{
#'      MELDNa = MELD(i) + 1.32(137-Na)-
#'        0.033\,MELD(i)(137-Na)
#'      }
#'    }
#'    
#' Extreme outliers will flag a warning from the package, these can manually be set higher or lower by the user.
#' 
#' The original MELD-Na model was described by Kim et al. (2008).
#' This function implements the subsequent 2016 MELD-Na formulation
#' adopted for liver allocation in the United States.
#' 
#' 
#' @references
#' Kim WR, Biggins SW, Kremers WK, Wiesner RH, Kamath PS, Benson JT,
#' Edwards E, Therneau TM. Hyponatremia and mortality among patients
#' on the liver-transplant waiting list. N Engl J Med.
#' 2008;359(10):1018-1026. doi:10.1056/NEJMoa0801209.
#'
#' United Network for Organ Sharing (UNOS). Policy and system changes
#' effective January 11, 2016, adding serum sodium to MELD calculation.
#'
#' @seealso [meld()] for the original MELD score
#' 
#' @param creatinine Serum creatinine in either mg/dL (US units) or μmol/L (SI units).
#' @param bilirubin  Serum total bilirubin in either mg/dL (US units) or μmol/L (SI units).
#' @param inr International normalized ratio of coagulation factor II, VII and X.
#' @param sodium Serum sodium in mmol/L.
#' @param unit Parameter setting if the measurements are based on "US" units (mg/dL) or SI units (μmol/L). Default value is "US". If the setting is changed to "SI", creatinine and bilirubin will be converted to mg/dL by multiplying their respective conversion factor.
#' @param dialysis Parameter setting if the patient is on hemodialysis at the time of measurement (default = "no"), if yes, then creatinine is hard-coded to 4.0 mg/dL before calculation.
#' @param creatinine_conversion_factor Factor multiplied to creatinine to convert from μmol/L to mg/dL (default = 0.0113).
#' @param bilirubin_conversion_factor Factor multiplied to bilirubin to convert from μmol/L to mg/dL (default = 0.0584).
#' @param creatinine_threshold_si Upper threshold for high creatinine (μmol/L), causing a warning if input is above (default = 200).
#' @param bilirubin_threshold_si Upper threshold for high bilirubin (μmol/L), causing a warning if input is above (default = 360).
#' @param creatinine_threshold_us Upper threshold for high creatinine (mg/dL), causing a warning if input is higher (default = 2.26).
#' @param bilirubin_threshold_us Upper threshold for high bilirubin (mg/dL), causing a warning if input is higher (default = 21.05).
#' @param sodium_upper_threshold Upper threshold for extremely high sodium measurements (default = 100)
#' @param sodium_lower_threshold Lower threshold for extremely low sodium levels (default = 160)
#' @param inr_threshold Upper threshold for INR, causing a warning if input is higher (default = 3.0).
#'
#' @returns A numeric vector of MELD-Na scores rounded to the nearest integer
#' @export
#'
#' @examples
#'   meldna(creatinine = 1, bilirubin = 1, inr = 1, sodium = 125, unit = "US", dialysis = "no") 
#'   # 6. 
#'   
#'   # This can be simplified to:
#'   meldna(1, 1, 1, 125)
#'   
#'   # SI units can be applied and are automatically converted to mg/dL
#'   meldna(120, 80, 2, 125, unit = "SI", dialysis = "no")
#'   # 30
#'   
#'   # If the patient has received haemodialysis, creatinine is hardcoded to 4.0. 
#'   # This setting can be enabled by setting "dialysis" to "yes"
#'   meldna(120, 80, 2, 125, unit = "SI", dialysis = "yes")
#'   # 36
#'   
meldna <- function(creatinine, bilirubin, inr, sodium, unit = "US", dialysis = "no",
                   creatinine_conversion_factor = 0.0113, bilirubin_conversion_factor = 0.0584,
                   creatinine_threshold_si = 200, bilirubin_threshold_si = 360,
                   creatinine_threshold_us = 2.26, bilirubin_threshold_us = 21.05,
                   sodium_upper_threshold = 160, sodium_lower_threshold = 100,
                   inr_threshold = 3.0){
  
  # Catch if unit or dialysis is formatted wrong
  if(!unit %in% c("US", "SI")){
    stop("unit must be either 'US' or 'SI'")
  }
  if(!dialysis %in% c("yes", "no")){
    stop("dialysis must be either 'yes' or 'no'")
  }
  
  # Warning if the user give extreme values 
  if(unit == "SI" && any(creatinine > creatinine_threshold_si |
                         bilirubin > bilirubin_threshold_si |
                         inr > inr_threshold | 
                         sodium > sodium_upper_threshold |
                         sodium < sodium_lower_threshold,
                         na.rm = TRUE)){
    warning("SI units detected: one or more lab values look unusual, consider if input is correct?")
  }
  else if(unit == "US" && any(creatinine > creatinine_threshold_us |
                              bilirubin > bilirubin_threshold_us |
                              inr > inr_threshold |
                              sodium > sodium_upper_threshold |
                              sodium < sodium_lower_threshold,
                              na.rm = TRUE)){
    warning("US units detected: one or more lab values look unusual, consider if input is correct?")
  }
  
  # If unit == SI then convert to mg/dL
  if(unit == "SI"){
    creatinine <- creatinine * creatinine_conversion_factor
    bilirubin <- bilirubin * bilirubin_conversion_factor

  }
  
  # If dialysis == yes, then hardcode creatinine to 4.0 mg/dL
  if(dialysis == "yes"){
    creatinine <- 4.0
  }
  
  # Set lower bounds on measured values
  creatinine <- pmax(creatinine, 1)
  bilirubin <- pmax(bilirubin, 1)
  inr <- pmax(inr, 1)
  
  # Sodium bound for MELD-Na
  sodium <- pmin(pmax(sodium, 125), 137)
  
  # Calculate MELD(i)
  meldi <- round(0.957 * log(creatinine) +
            0.378 * log(bilirubin) +
            1.120 * log(inr) + 0.643, digits = 10) * 10
  
  # If MELD(i) > 11, then calculate MELDNa
  index <- meldi > 11
  
  
  meldi[index] <- meldi[index] + 1.32 * (137 - sodium[index]) -
      ((0.033 * meldi[index]) * (137 - sodium[index]))
  
  round(meldi)
}