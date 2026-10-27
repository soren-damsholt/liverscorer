#' Model for End-stage Liver Disease - Sodium score
#'
#' Calculates the Model for End-Stage Liver Disease incorporating serum sodium (MELD-Na) score. 
#' The calculation follows the 2016 MELD-Na formulation
#' 
#' @details
#' The MELD-Na score is calculated from S-creatinine, S-bilirubin, INR and S-sodium.
#' Creatinine, bilirubin and INR are bounded so values < 1 are set to 1. Sodium is bounded between
#' 125 and 137 Both US and SI units are applicable for this function. If SI units is selected, the units are automatically converted to mg/dl before calculation.
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
#' If one or more required input values are missing for an observation, the corresponding MELD-Na score is returned as "Na"
#' 
#' The original MELD-Na model was described by Kim et al. (2008).
#' This function implements the subsequent 2016 MELD-Na formulation
#' adopted for livertransplant allocation in the United States.
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
#' @param digits Integer setting the number of digits the final score is rounded to (default = 0)
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
                   digits = 0){
  
  # Catch if unit or dialysis is formatted wrong
  .check_scalar_choice(unit, c("SI", "US"), "unit")
  
  .check_scalar_choice(dialysis, c("yes", "no"), "dialysis")
  
  # Catch if numeric values are non-numeric:
  numeric_inputs <- list(
    creatinine = creatinine,
    bilirubin = bilirubin,
    inr = inr,
    sodium = sodium
  )
  
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))){
    stop("Creainine, inr, bilirubin and sodium must be numeric")
  }
  
  # Locate missing input values
  missing <- is.na(creatinine) | 
              is.na(bilirubin) |
              is.na(inr) |
              is.na(sodium)
  
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
  creatinine <- pmin(pmax(creatinine, 1), 4)
  bilirubin <- pmax(bilirubin, 1)
  inr <- pmax(inr, 1)
  
  # Sodium bound for MELD-Na
  sodium <- pmin(pmax(sodium, 125), 137)
  
  # Calculate MELD(i)
  meldi <- round(0.957 * log(creatinine) +
            0.378 * log(bilirubin) +
            1.120 * log(inr) + 0.643, digits = 10) * 10
  
  # If MELD(i) > 11, then calculate MELDNa
  index <- !is.na(meldi) & meldi > 11
  
  
  meldi[index] <- meldi[index] + 1.32 * (137 - sodium[index]) -
      ((0.033 * meldi[index]) * (137 - sodium[index]))
  
  meldi[missing] <- NA_real_
  
  round(meldi, digits = digits)
}
