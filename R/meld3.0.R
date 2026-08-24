#' Title Model for End-stage Liver disease 3.0 (score)
#' 
#' Vectorized function for calculating the MELD 3.0 score.
#' 
#' @details
#' MELD 3.0. score is calcluated from sex, age, creatinine, bilirubin, INR, sodium and albumin.
#' The adult MELD 3.0 calculation follows Kim et al. (2021). Subjects aged 12-17 years, the sex-independent formulation used by the online calculator is applied.
#' Therfore, creatinine is bounded between 1-3 mg/dL, and if the dialysis option is set, it is hardcoded as 3 mg/dL
#' Bilirubin and INR have a lower bound of 1, so measurements below 1 are hardcoded to 1. 
#' Sodium is bounded between 125 and 137, and albumin is bounded between 1.5 and 3.5 g/dL.
#' MELD 3.0 follow the following formula:
#'  \enumerate{
#'    \item If the patient age is ≥ 18, the following formula is used:
#'    \deqn{MELD 3.0 = (1.33 \times female) +
#'          4.56 \times \log(bilirubin) +
#'          0.82 \times (137 - sodium) -
#'          (0.24 \times (137 - sodium) \times \log(bilirubin)) + 
#'          9.09 \times \log(inr) + 
#'          11.14 \times \log(creatinine) +
#'          1.85 \times (3.5 - albumin) -
#'          (1.83 \times (3.5 - albumin) \times \log(creatinine)) + 6
#'    
#'    }
#'    \item If the patient age is 12-17, the sex-independent formula is used:
#'    \deqn{MELD 3.0 = 4.56 \times \log(bilirubin) +
#'          0.82 \times (137 - sodium) -
#'          (0.24 \times (137 - sodium) \times \log(bilirubin)) + 
#'          9.09 \times \log(inr) +
#'          11.14 \times \log(creatinine) +
#'          1.85 \times (3.5 - albumin) -
#'          (1.83 \times (3.5 - albumin) * \log(creatinine)) + 7.33}
#'    }
#'  
#'  MELD 3.0 is applicable to candidates aged 12 years or older, the function will stop if any age is below 12 years.
#'  Missing input values are automatically handled as NA values, and will produce NAs.
#' 
#' @references Kim WR, Mannalithara A, Heimbach JK, Kamath PS, Asrani SK, Biggins SW, Wood NL, Gentry SE, Kwong AJ. MELD 3.0: The Model for End-Stage Liver Disease Updated for the Modern Era. Gastroenterology. 2021 Dec;161(6):1887-1895.e4. doi: 10.1053/j.gastro.2021.08.050. Epub 2021 Sep 3. PMID: 34481845; PMCID: PMC8608337.
#'
#' @seealso [meld()], [meldna()]
#' 
#' @param sex Character vector specifying sex as either "male" or "female".
#' @param age Numeric vector of Age in years.
#' @param creatinine Numeric vector of creatinine as either µmol/L or mg/dL.
#' @param bilirubin Numeric vector of bilirubin as either µmol/L or mg/dL.
#' @param inr International normalized ratio (INR) of prothrombin time.
#' @param sodium Numeric vector of sodium measured as either mmol/L or mEq/L
#' @param albumin Numeric vector of albumin measured as either g/L or g/dL
#' @param dialysis Setting of if the patient has received ≥2 dialysis treatments during the 7 days preceding creatinine measurement or ≥24 hours of continuous veno-venous hemodialysis preceding the creatinine measurement (Default = FALSE).
#' @param unit Character specifying the units of the laboratory measurements.
#'              '"US"' expects creatinine and bilirubin in mg/dL and albumin in g/dL.
#'              '"SI"' expects creatinine and bilirubin in µmol/L and albumin in g/L.
#'             Sodium is entered in mmol/L (numerically equivalent to mEq/L) in each setting.
#' @param creatinine_conversion_factor Factor multiplied to creatinine to convert from μmol/L to mg/dL (default = 0.0113).
#' @param bilirubin_conversion_factor Factor multiplied to bilirubin to convert from μmol/L to mg/dL (default = 0.0584).
#' @param albumin_conversion_factor Factor multiplied to albumin to convert from g/L to g/dL (default = 0.1)
#'
#' @returns Numeric vector of MELD 3.0 score rounded to nearest integer
#' @export
#'
#' @examples
#'   meld3(sex = "female", age = 22, creatinine = 1, bilirubin = 1, inr = 1, sodium = 125, albumin = 3.6, dialysis = FALSE, unit = "US")
#'   # 17
#'   
#'   # This can be simplified to:
#'   meld3("female", 22, 1, 1, 1, 125, 3.6)
#'   
#'   # SI units can be applied and are automatically converted to mg/dL and g/dL 
#'   meld3(sex = "female", age = 22, creatinine = 88, bilirubin = 17, inr = 1, sodium = 125, albumin = 36, unit = "SI")
#'   # 17
#'   
#'   # If the patient meets the dialysis criteria, creatinine is set to 3.0 mg/dL.
#'   # This setting can be enabled by setting "dialysis" to TRUE
#'   meld3(sex = "female", age = 22, creatinine = 88, bilirubin = 17, inr = 1, sodium = 125, albumin = 36, unit = "SI", dialysis = TRUE)
#'   # 29
#'   
#'   # If the patient is aged below 18, the sex-independent formula is applied
#'   meld3(sex = "male", age = 16, creatinine = 112, bilirubin = 80, inr = 2.0, sodium = 136, albumin = 35, unit = "SI", dialysis = FALSE)
#'   # 24
#'   meld3(sex = "male", age = 22, creatinine = 112, bilirubin = 80, inr = 2.0, sodium = 136, albumin = 35, unit = "SI", dialysis = FALSE)
#'   # 22

meld3 <- function(sex, age, creatinine, bilirubin, inr, sodium, albumin, dialysis = FALSE, unit = "US",
                  creatinine_conversion_factor = 0.0113, bilirubin_conversion_factor = 0.0584,
                  albumin_conversion_factor = 0.1){

  # Input validation
  if(!all(sex %in% c("male", "female", NA))) {
    stop("sex must be either 'male' or 'female'")
  }
  
  if(any(!is.numeric(age) & !is.na(age) |
         !is.numeric(creatinine) & !is.na(creatinine) |
         !is.numeric(bilirubin) & !is.na(bilirubin) |
         !is.numeric(inr) & !is.na(inr) |
         !is.numeric(albumin) & !is.na(albumin) |
         !is.numeric(sodium) & !is.na(sodium))) {
    stop("One or more of your vectors are not numeric")
  }
  
  if(!unit %in% c("US", "SI")) {
    stop("unit must be either 'US' or 'SI'")
  }
  
  if(!all(age >= 12 | is.na(age))){
    stop("MELD 3.0 is applicable to candidates aged 12 years and older")
  }
  
  if(!is.logical(dialysis) || anyNA(dialysis)){
     stop("dialysis must be TRUE or FALSE")
   }
 
  # Vector length validation (i.e. require that each input has length 1 or a common max length).
  # Except dialysis which may be scalar (if length == 1).
  input_lengths <- c(length(sex), 
                     length(age), 
                     length(creatinine),
                     length(bilirubin), 
                     length(inr), 
                     length(sodium),
                     length(albumin))
  
  n <- input_lengths[1]

  if(n == 0) {
    stop("Input vectors must contain at least one observation")
  }
 
  if (length(unique(input_lengths)) != 1){
    stop("All input vectors must have the same length")
  }
  
  if(length(dialysis) != 1 && length(dialysis) != n) {
    stop("dialysis must have length 1 or the same length as the input vectors")
  }
  


  
  # Missing values
  missing <- is.na(sex) |
              is.na(age) |
              is.na(creatinine) |
              is.na(bilirubin) |
              is.na(inr) |
              is.na(sodium) |
              is.na(albumin)
   
  # unit conversion to mg/dL
  if(unit == "SI"){
    creatinine <- creatinine * creatinine_conversion_factor
    bilirubin <- bilirubin * bilirubin_conversion_factor
    albumin <- albumin * albumin_conversion_factor
  }
  
  # Bounded values
  creatinine <- pmin(pmax(creatinine, 1), 3)
  bilirubin <- pmax(bilirubin, 1) 
  inr <- pmax(inr, 1)
  sodium <- pmin(pmax(sodium, 125), 137)
  albumin <- pmin(pmax(albumin, 1.5), 3.5) 

  # If dialysis rule is enabled, then set creatinine to 3.0 mg/dL.
  creatinine[dialysis] <- 3.0
  
  # female sex?
  female <- as.numeric(sex == "female")

  # formula
  common <- 4.56 * log(bilirubin) +
            0.82 * (137 - sodium) -
            (0.24 * (137 - sodium) * log(bilirubin)) + 
            9.09 * log(inr) +
            11.14 * log(creatinine) +
            1.85 * (3.5 - albumin) -
            (1.83 * (3.5 - albumin) * log(creatinine))
  
  meld3 <- common + 7.33
  
  adult <- !is.na(age) & age >= 18
  
  meld3[adult] <- common[adult] + 6 + (1.33 * female[adult])
    
  meld3[missing] <- NA_real_
  
  round(meld3, digits = 0)
}

