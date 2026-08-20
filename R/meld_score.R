#' Title Model of End-Stage Liver Disease (MELD) score
#'
#' @param creatinine Serum creatinine in either mg/dL (US units) or μmol/L (SI units).
#' @param bilirubin Serum total bilirubin in either mg/dL (US units) or μmol/L (SI units).
#' @param inr International normalized ratio of coagulation factor II, VII and X.
#' @param dialysis Parameter setting if the patient is on hemodialysis at the time of measurement (default = "no"), if yes, then creatinine is hard-coded to 4.0 mg/dL before calculation.
#' @param unit Parameter setting if the measurements are based on "US" units (mg/dL) or SI units (μmol/L). Default value is "US". If the setting is changed to "SI", creatinine and bilirubin will be converted to mg/dL by multiplying their respective conversion factor.
#' @param creatinine_conversion_factor Factor multiplied to creatinine to convert from μmol/L to mg/dL (default = 0.0113).
#' @param bilirubin_conversion_factor Factor multiplied to bilirubin to convert from μmol/L to mg/dL (default = 0.0584).
#' @param creatinine_threshold_si Upper threshold for high creatinine (μmol/L), causing a warning if input is above (default = 200).
#' @param bilirubin_threshold_si Upper threshold for high bilirubin (μmol/L), causing a warning if input is above (default = 360).
#' @param creatinine_threshold_us Upper threshold for high creatinine (mg/dL), causing a warning if input is higher (default = 2.26).
#' @param bilirubin_threshold_us Upper threshold for high bilirubin (mg/dL), causing a warning if input is higher (default = 21.05).
#' @param inr_threshshold Upper threshold for INR, causing a warning if input is higher (default = 3.0).
#'
#' @returns Function returns a calulated MELD value, rounded to nearest integer.
#' @export
#'
#' @examples
#'   # US units (mg/dL)
#'   meld(creatinine = 1.2, bilirubin = 3.0, inr = 2.0, unit = "US", dialysis = "no") 
#'   # should return 20
#'   
#'   # or simply:
#'   meld(1.2, 3.0, 2.0)
#'   
#'   #If the patient received dialysis at the time of sampling,
#'   #creatinine is hard-coded to 4.0, regardless of measured value.
#'   meld(creatinine = 1.2, bilirubin = 3.0, inr = 2.0, unit = "US", dialysis = "yes") 
#'   # should return 32
#'   
#'   #If you are using SI units, you need to change the "unit" setting to "SI", 
#'   #which will then automatically convert your values before caluculating a MELD score.
#'   meld(creatinine = 120, bilirubin = 80, inr = 2.0, unit = "SI") # should return 23
meld <- function(creatinine, bilirubin, inr, dialysis = "no", unit = "US",
                 creatinine_conversion_factor = 0.0113, bilirubin_conversion_factor = 0.0584,
                 creatinine_threshold_si = 200, bilirubin_threshold_si = 360,
                 creatinine_threshold_us = 2.26, bilirubin_threshold_us = 21.05, 
                 inr_threshshold = 3.0){
  #Input check
  if(!unit %in% c("US", "SI")){
    stop("unit must be either 'US' or 'SI'")
  }
  if(!dialysis %in% c("yes", "no")){
    stop("dialysis must be either 'yes' or 'no'")
  }
  # Safety check
  if(unit == "SI" && any(creatinine > creatinine_threshold_si |
                      bilirubin > bilirubin_threshold_si |
                      inr > inr_threshshold,
                      na.rm = TRUE)){
    warning("SI units detected: some lab values are high, consider if input is correct?")
  }
  else if(unit == "US" && any(creatinine > creatinine_threshold_us |
                           bilirubin > bilirubin_threshold_us |
                           inr > inr_threshshold,
                           na.rm = TRUE)){
    warning("US units detected: some lab values are high, consider if input is correct?")
  }
  
  # Convert SI units to mg/dL
  if(unit == "SI"){
    creatinine <- creatinine * creatinine_conversion_factor
    bilirubin <- bilirubin * bilirubin_conversion_factor
  }
  
  # If dialysis
  if(dialysis == "yes"){
    creatinine <- 4.0
  }
  
  # Set MELD minimum values
  creatinine <- pmax(creatinine, 1)
  bilirubin <- pmax(bilirubin, 1)
  inr <- pmax(inr, 1)
  
  # Calculate meld
  meld <- round((0.957 * log(creatinine) +
     0.378 * log(bilirubin)+
     1.120 * log(inr) +
     0.643)*10, digits = 0)
    
  return(meld)
}
