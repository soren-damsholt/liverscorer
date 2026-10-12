#' Title Model for End-Stage Liver Disease (MELD) score
#' 
#' Calculates the Model for End-Stage Liver Disease (MELD).
#' 
#' @details
#' The MELD score is calculated from s-Creatinine, s-Bilirubin and s-inr.
#' Creatinine is bounded between 1 and 4 mg/dL, while bilirubin and INR are bounded below at 1. 
#' Both US and SI units are applicable for this function, where SI units are automatically converted to mg/dl before calculation.
#' If the patient has undertaken haemodialysis within 48 h of sample collection, the setting dialysis should be set to "yes", in which case creatinine is hardcoded to 4 (mg/dL).
#' The MELD calculation follows this formula:
#' \enumerate{
#'  \deqn{
#'  MELD = 10 \times round(
#'    0.957 \times \log(Creatinine) + 
#'    0.378 \times \log(Bilirubin)+
#'    1.120\times \log(INR) + 0.643, digits = 10)}}
#' 
#' If one or more of the required input values are missing for an observation, the corresponding MELD values is returned as "NA".
#' 
#' @references
#' Kamath PS, Wiesner RH, Malinchoc M, et al. A model to predict survival
#' in patients with end-stage liver disease. Hepatology. 2001;33(2):464-470.
#'
#' Kim WR, Mannalithara A, Heimbach JK, et al. MELD 3.0: The Model for
#' End-Stage Liver Disease Updated for the Modern Era. Gastroenterology.
#' 2021;161(6):1887-1895.e4. doi:10.1053/j.gastro.2021.08.017.
#' 
#' @seealso [meldna()], [meld3()]
#'
#' @param creatinine Serum creatinine in either mg/dL (US units) or μmol/L (SI units).
#' @param bilirubin Serum total bilirubin in either mg/dL (US units) or μmol/L (SI units).
#' @param inr International normalized ratio of coagulation factor II, VII and X.
#' @param dialysis Parameter setting if the patient is on hemodialysis at the time of measurement (default = "no"), if yes, then creatinine is hard-coded to 4.0 mg/dL before calculation.
#' @param unit Parameter setting if the measurements are based on "US" units (mg/dL) or SI units (μmol/L). Default value is "US". If the setting is changed to "SI", creatinine and bilirubin will be converted to mg/dL by multiplying their respective conversion factor.
#' @param creatinine_conversion_factor Factor multiplied to creatinine to convert from μmol/L to mg/dL (default = 0.0113).
#' @param bilirubin_conversion_factor Factor multiplied to bilirubin to convert from μmol/L to mg/dL (default = 0.0584).
#' @param digits Integer setting the number of digits the final score is rounded to (default = 0)
#'
#' @returns A numeric vector of MELD scores, rounded to the nearest integer.
#' @export
#'
#' @examples
#'   # US units (mg/dL)
#'   meld(creatinine = 1.2, bilirubin = 3.0, inr = 2.0, unit = "US", dialysis = "no") 
#'   # 20
#'   
#'   # or simply:
#'   meld(1.2, 3.0, 2.0)
#'   
#'   #If the patient received dialysis at the time of sampling,
#'   #creatinine is hard-coded to 4.0, regardless of measured value.
#'   meld(creatinine = 1.2, bilirubin = 3.0, inr = 2.0, unit = "US", dialysis = "yes") 
#'   # 32
#'   
#'   #If you are using SI units, you need to change the "unit" setting to "SI", 
#'   #which will then automatically convert your values before caluculating a MELD score.
#'   meld(creatinine = 120, bilirubin = 80, inr = 2.0, unit = "SI") # should return 23
meld <- function(creatinine, bilirubin, inr, dialysis = "no", unit = "US",
                 creatinine_conversion_factor = 0.0113,
                 bilirubin_conversion_factor = 0.0584,
                 digits = digits){
  #Input check
  if(!unit %in% c("US", "SI")){
    stop("unit must be either 'US' or 'SI'")
  }
  if(!dialysis %in% c("yes", "no")){
    stop("dialysis must be either 'yes' or 'no'")
  }
  # Locate missing values
  missing <- is.na(creatinine) |
                   is.na(bilirubin) |
                   is.na(inr)
  
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
  creatinine <- pmin(pmax(creatinine, 1), 4)
  bilirubin <- pmax(bilirubin, 1)
  inr <- pmax(inr, 1)
  
  
  # Calculate meld
  meld <- (0.957 * log(creatinine) +
     0.378 * log(bilirubin)+
     1.120 * log(inr) +
     0.643)*10
  
  meld[missing] <- NA_real_
    
  return(round(meld, digits = digits))
}
