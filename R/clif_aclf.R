#' CLIF-C ACLF score
#'
#' Vectorized function for calculating the CLIF-C ACLF score
#' @details Calculates the CLIF-C ACLF score described by Jalan et al. (2014).
#'   The function first calculates the CLIF-C Organ Failure (CLIF-C OF) score
#'   using [clif_of()] and then applies the CLIF-C ACLF prognostic formula:
#' \deqn{10 \times \left(0.33 \times CLIF\text{-}OF + 0.04 \times age +
#' 0.63 \times \log(WBC) - 2 \right)}
#'
#' where age is measured in years, WBC is measured in \eqn{10^9/L}, and
#' \eqn{\log} denotes the natural logarithm.
#'
#'   The score is bounded between 0 and 100, as described in the original
#'   reference by Jalan et al. Please note the specifications for calculating
#'   the CLIF-C OF score in [clif_of()]. If any required patient-level measurement
#'   is missing, the corresponding total CLIF-C ACLF score is returned as `NA`.
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
#'   Engelmann, C., Thomsen, K.L., Zakeri, N. et al.
#'   Validation of CLIF-C ACLF score to define a threshold for futility of
#'   intensive care support for patients with acute-on-chronic liver failure.
#'   Crit Care 22, 254 (2018). https://doi.org/10.1186/s13054-018-2156-0
#' 
#' @seealso [clif_of()] [clif_ad()]
#' 
#' @param age Numeric vector of age in years.
#' @param wbc Numeric vector of white blood cell count in \eqn{10^9/L}.
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
#' @param units Character scalar specifying whether bilirubin and creatinine are
#'   supplied in 'SI' units or 'US' units (default). If 'SI' is chosen, the
#'   values are automatically converted to mg/dL in the function.
#' @param oxygen_ratio_type Character scalar specifying whether `oxygen_ratio`
#'   represents the \eqn{PaO_2/FiO_2} ratio (`"pao2"`, default) or the
#'   \eqn{SpO_2/FiO_2} ratio (`"spo2"`).
#' @param mechanical_ventilation Logical scalar or vector indicating mechanical
#'   ventilation for respiratory failure. When `TRUE`, the respiratory subscore
#'   is set to 3. For patients mechanically ventilated solely because of hepatic
#'   encephalopathy, set `mechanical_ventilation = FALSE` and set
#'   `encephalopathy` to 3 or 4 so that the cerebral subscore is 3.
#' @param digits Non-negative integer specifying the number of decimal places
#'   used to round the final score. Default is 0.
#' @returns Numeric vector of CLIF-C ACLF scores bounded between 0 and 100
#'   and rounded to `digits` decimal places.
#' @export
#'
#' @examples
#' clif_aclf(age = c(60, 80, 90), wbc = c(30, 50, 80),
#'  bilirubin = c(80, 20, 250), creatinine = c(80, 120, 130),
#'   encephalopathy = c(1, 2, 3), inr = c(2, 1, 2.5),
#'    map = c(70, 65, 66), oxygen_ratio = c(340, 280, 240),
#'     rrt = FALSE, vasopressors = FALSE, units = "SI",
#'      oxygen_ratio_type = "pao2", mechanical_ventilation = FALSE)
#' # 52 66 90
clif_aclf <- function(age, wbc, bilirubin, creatinine, encephalopathy,
                      inr, map, oxygen_ratio, oxygen_ratio_type = "pao2",
                      rrt = FALSE, vasopressors = FALSE, mechanical_ventilation = FALSE,
                      units = "US", digits = 0) {
  # INPUT VALIDATION:
  numeric_inputs <- list(
    age = age,
    wbc = wbc
  )
  
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))){
    stop("Age and white blood cell count(wbc) must be numeric")
  }
  
  if (!is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      digits < 0 ||
      digits %% 1 != 0) {
    stop("digits must be a single non-negative integer")}
  
  if (length(age) == 0L || length(wbc) == 0L) {
    stop("age and wbc must contain at least one observation")}
  
  if (any(age <= 0, na.rm = TRUE)) {
    stop("age must be greater than 0")}
  
  if (any(wbc <= 0, na.rm = TRUE)) {
    stop("wbc must be greater than 0")}
  
  # CALCULATION
  ## CLIF OF SCORE:
  clif_of_score <- clif_of(bilirubin = bilirubin, creatinine = creatinine,
                           inr = inr, encephalopathy = encephalopathy,
                           map = map, oxygen_ratio = oxygen_ratio,
                           oxygen_ratio_type = oxygen_ratio_type, rrt = rrt,
                           mechanical_ventilation = mechanical_ventilation,
                           vasopressors = vasopressors,
                           units = units)
  
  ## Input length validation
  input_lengths <- c(age = length(age),
                     wbc = length(wbc),
                     clif_of = length(clif_of_score))
  
  if (length(unique(input_lengths)) != 1L) {
    stop("age, wbc and CLIF-C OF inputs must have the same length")}
  
  ## MISSING VALUES
  missing <- is.na(clif_of_score) | 
             is.na(age) |
             is.na(wbc)
  
  ## CLIF ACLF SCORE  
  clif_c_aclf <- 10 * (0.33 * clif_of_score +
                       0.04 * age +
                       0.63 * log(wbc) - 2)
  
  ## Score is bounded between 0 and 100
  clif_c_aclf <- pmax(0, pmin(100, clif_c_aclf))
  
  clif_c_aclf[missing] <- NA_real_
  round(clif_c_aclf, digits = digits)
}

