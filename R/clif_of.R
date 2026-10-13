#' CLIF-C Organ Failure (OF) score
#' 
#' Vectorized function for calculating the CLIF-C OF score.
#'  
#' @details Calculates the CLIF-C Organ Failure (OF) score described by Jalan et
#' al. (2014). Six organ systems are assigned subscores from 1 to 3: liver,
#' kidney, coagulation, brain, circulation, and respiration. The subscores are
#' summed to produce a total score ranging from 6 to 18.
#'
#' Respiratory function may be assessed using either the \eqn{PaO_2/FiO_2} or
#' \eqn{SpO_2/FiO_2} ratio. Set `oxygen_ratio_type` accordingly.
#'
#' Patients mechanically ventilated because of respiratory failure receive a
#' respiratory subscore of 3. For patients mechanically ventilated solely
#' because of hepatic encephalopathy, `mechanical_ventilation` should be `FALSE`
#' and `encephalopathy` should be set to 3 or 4, yielding a cerebral subscore
#' of 3.
#'
#' If any required patient-level measurement is missing, the corresponding total
#' CLIF-C OF score is returned as `NA`.
#'
#' @references Jalan R, Saliba F, Pavesi M, Amoros A, Moreau R, Ginès P,
#'   Levesque E, Durand F, Angeli P, Caraceni P, Hopf C, Alessandria C,
#'   Rodriguez E, Solis-Muñoz P, Laleman W, Trebicka J, Zeuzem S, Gustot T,
#'   Mookerjee R, Elkrief L, Soriano G, Cordoba J, Morando F, Gerbes A, Agarwal
#'   B, Samuel D, Bernardi M, Arroyo V; CANONIC study investigators of the
#'   EASL-CLIF Consortium. Development and validation of a prognostic score to
#'   predict mortality in patients with acute-on-chronic liver failure. J
#'   Hepatol. 2014 Nov;61(5):1038-47. doi: 10.1016/j.jhep.2014.06.012. Epub 2014
#'   Jun 17. PMID: 24950482.
#'
#'   Engelmann, C., Thomsen, K.L., Zakeri, N. et al. Validation of CLIF-C ACLF
#'   score to define a threshold for futility of intensive care support for
#'   patients with acute-on-chronic liver failure. Crit Care 22, 254 (2018).
#'   https://doi.org/10.1186/s13054-018-2156-0
#'
#' @seealso [clif_ad()] [clif_aclf()]
#'
#' @param bilirubin Numeric vector of bilirubin in mg/dL (default) or \eqn{\mu
#'   mol/L} when `units = "SI"`.
#' @param creatinine Numeric vector of creatinine in mg/dL (default) or \eqn{\mu
#'   mol/L} when `units = "SI"`.
#' @param inr Numeric vector of International normalized ratio.
#' @param encephalopathy Numeric vector specifying the degree of encephalopathy,
#'   graded by the West-Haven grade.
#' @param map Numeric vector of mean arterial pressure (MAP) in mmHg.
#' @param oxygen_ratio Numeric vector of either the \eqn{PaO_2/FiO_2} ratio or
#'   the \eqn{SpO_2/FiO_2} ratio, as specified by `oxygen_ratio_type`
#' @param rrt Logical scalar or vector indicating renal replacement therapy
#'   (RRT). A scalar applies to all observations; otherwise the vector must have
#'   the same length as the patient-level inputs. Default is `FALSE`.
#' @param vasopressors Logical scalar or vector indicating use of vasopressors.
#'   A scalar applies to all observations; otherwise the vector must have the
#'   same length as the patient-level inputs. Default is `FALSE`.
#' @param mechanical_ventilation Logical scalar or vector indicating mechanical
#'   ventilation for respiratory failure. When `TRUE`, the respiratory subscore
#'   is set to 3. For patients mechanically ventilated solely because of hepatic
#'   encephalopathy, set `mechanical_ventilation = FALSE` and set
#'   `encephalopathy` to 3 or 4 so that the cerebral subscore is 3.
#' @param oxygen_ratio_type Character scalar specifying whether `oxygen_ratio`
#'   represents the \eqn{PaO_2/FiO_2} ratio (`"pao2"`, default) or the
#'   \eqn{SpO_2/FiO_2} ratio (`"spo2"`).
#' @param units Character scalar specifying whether bilirubin and creatinine are
#'   supplied in 'SI' units or 'US' units (default). If 'SI' is chosen, the
#'   values are automatically converted to mg/dL in the function.
#' @param bilirubin_conversion_factor Factor multiplied to bilirubin to convert
#'   from μmol/L to mg/dL (default = 0.0584).
#' @param creatinine_conversion_factor Factor multiplied to creatinine to
#'   convert from μmol/L to mg/dL (default = 0.0113).
#'
#' @returns Integer vector of CLIF-C Organ Failure scores ranging from 6 to 18.
#' @export
#'
#' @examples
#' clif_of(6, 2, 2, 2, 70, 340, rrt = FALSE, vasopressors = FALSE, units = "US",
#'  oxygen_ratio_type = "pao2", mechanical_ventilation = FALSE)
#' # 10
#'
#' # The function handles SI units and internally converts them to mg/dL before calculating the score.
#' clif_of(120, 80, 2, 2, 70, 340, rrt = FALSE, vasopressors = FALSE, units = "SI",
#'  oxygen_ratio_type = "pao2", mechanical_ventilation = FALSE)
#' #9
#'
#' # This function is fully vectorized, so it can calculate multiple values at once:
#' clif_of(bilirubin = c(12,8,6) , creatinine = c(3.3, 2, 1), inr = c(2, 1.8, 1.2),
#'  encephalopathy = c(0, 2, 4), map = c(60, 70, 80), rrt = c(FALSE, TRUE, FALSE),
#'   vasopressors = c(TRUE, FALSE, TRUE), units = "US", oxygen_ratio_type = "spo2",
#'    oxygen_ratio = c(358, 350, 214), mechanical_ventilation = c(FALSE, TRUE, TRUE))
#' # 12 12 13
clif_of <- function(bilirubin, creatinine, inr, encephalopathy,
                    map, oxygen_ratio, 
                    rrt = FALSE, vasopressors = FALSE,
                    mechanical_ventilation = FALSE,
                    oxygen_ratio_type = "pao2", units = "US",
                    bilirubin_conversion_factor = 0.0584,
                    creatinine_conversion_factor = 0.0113) {
  
  # Input validation
  ## SCALAR SETTINGS
  .check_scalar_choice(units, c("US", "SI"), "units")
  .check_scalar_choice(oxygen_ratio_type, c("pao2", "spo2"), "oxygen_ratio_type")
  
  ## NUMERIC INPUTS
  numeric_inputs <- list(
    bilirubin = bilirubin,
    creatinine = creatinine,
    inr = inr,
    encephalopathy = encephalopathy,
    map = map,
    oxygen_ratio = oxygen_ratio)
  
  ## NUMERIC TYPES
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))){
    stop("Bilirubin, creatinine, INR, West-Haven grade, MAP and PaO2/SpO2 must be numeric")
  }
  ## CATEGORICAL SETTINGS/LENGTH
  if(any(!is.na(encephalopathy) &
         !encephalopathy %in% 0:4)) {
    stop("Encephalopathy (West-Haven grade) should be an integer between 0 and 4")
  }
  
  .check_logical(rrt, "Renal replacement therapy (rrt)")
  .check_logical(vasopressors, "Vasopressors")
  .check_logical(mechanical_ventilation, "Ventilation")
  
  input_lengths <- vapply(numeric_inputs, length, integer(1))
  if (any(input_lengths == 0L)) {
    stop("Input vectors must contain at least one observation")}
  
  if (length(unique(input_lengths)) != 1L) {
    stop("All patient-level input vectors must have the same length")}
  
  n <- input_lengths[1]
  .check_scalar_or_length(rrt, n, "Renal replacement therapy (rrt)")
  .check_scalar_or_length(vasopressors, n, "Vasopressors")
  .check_scalar_or_length(mechanical_ventilation, n, "Ventilation")
  
  # EXPAND INTENTIONAL SCALAR SETTINGS:
  if(length(rrt) == 1L) {
    rrt <- rep(rrt, n)
  }
  if(length(vasopressors) == 1L) {
    vasopressors <- rep(vasopressors, n)
  }
  if(length(mechanical_ventilation) == 1L) {
    mechanical_ventilation <- rep(mechanical_ventilation, n)
  }
  
  # Unit validation 
  if(any(bilirubin <= 0, na.rm = TRUE) ||
     any(creatinine <= 0, na.rm = TRUE) ||
     any(inr <= 0, na.rm = TRUE) ||
     any(map <= 0, na.rm = TRUE) ||
     any(oxygen_ratio <= 0, na.rm = TRUE)
     ) {
    stop("Bilirubin, creatinine, INR, MAP and PaO2/SpO2 must all be greater than 0")
  }
  
  # UNIT CONVERSION: SI units to US
  if(units == "SI") {
    bilirubin <- bilirubin * bilirubin_conversion_factor
    creatinine <- creatinine * creatinine_conversion_factor
  }
  
  # OF SCORE CALCULATION:
  ## Liver failure
  liver <- rep(NA_integer_, length(bilirubin))
  liver[which(bilirubin < 6)] <- 1L
  liver[which(bilirubin >=6 & bilirubin < 12)] <- 2L
  liver[which(bilirubin >= 12)] <- 3L
  
  ## Kidney failure
  kidney <- rep(NA_integer_, length(creatinine))
  kidney[which(rrt | creatinine >= 3.5)] <- 3
  kidney[which(creatinine >= 2 & creatinine < 3.5 & !rrt)] <- 2
  kidney[which(creatinine < 2 & !rrt)] <- 1
  
  ## Coagulation failure
  coag <- rep(NA_integer_, length(inr))
  coag[which(inr >= 2.5)] <- 3
  coag[which(inr >= 2 & inr < 2.5)] <- 2
  coag[which(inr < 2)] <- 1
    
  ## Brain failure
  brain <- rep(NA_integer_, length(encephalopathy))
  brain[which(encephalopathy >= 3)] <- 3
  brain[which(encephalopathy >= 1 & encephalopathy < 3)] <- 2
  brain[which(encephalopathy == 0)] <- 1
  
  ## Circulation failure
  circulation <- rep(NA_integer_, length(map))
  circulation[which(vasopressors)] <- 3
  circulation[which(map < 70 & !vasopressors)] <- 2
  circulation[which(map >= 70 & !vasopressors)] <- 1
  
  ## Respiratory failure
  respiration <- rep(NA_integer_, length(oxygen_ratio))
  if(oxygen_ratio_type == "pao2") {
  respiration[which(oxygen_ratio > 300)] <- 1
  respiration[which(oxygen_ratio > 200 & oxygen_ratio <= 300)] <- 2
  respiration[which(oxygen_ratio <= 200 | mechanical_ventilation)] <- 3
  } else if(oxygen_ratio_type == "spo2"){
  respiration[which(oxygen_ratio > 357)] <- 1
  respiration[which(oxygen_ratio > 214 & oxygen_ratio <= 357)] <- 2
  respiration[which(oxygen_ratio <= 214 | mechanical_ventilation)] <- 3
  }

  of_score <- liver + kidney + coag + brain + circulation + respiration
  of_score
}


