#' CLIF-C Acute Decompensation (AD) score
#'
#' Vectorized function for calculating the CLIF-C Acute Decompensation (CLIF-C
#' AD) score.
#'
#' @details Calculates the CLIF-C Acute Decompensation (CLIF-C AD) score, as 
#' described by Jalan et al. (2015). The score was developed as a prognostic tool
#' for assessing hospitalized patients with acutely decompensated cirrhosis, but 
#' not ACLF. If the patient has ACLF you should consider the [clif_aclf()] score.
#' The CLIF-C AD score is calculated as:
#' \deqn{10 \times \left(0.03 \times age + 0.66 \times \log(creatinine) +
#' 1.71 \times \log(INR) + 0.88 \times \log(WBC) - 0.05 \times sodium + 8 \right)
#' }
#'
#' where age is measured in years, creatinine in mg/dL, WBC in
#' \eqn{10^9/L}, sodium in mmol/L, and \eqn{\log} denotes the natural
#' logarithm.
#'
#' When unit = "SI", creatinine is assumed to be supplied in
#' \eqn{\mu mol/L} and is converted internally to mg/dL before calculation.
#' The final score is bounded between 0 and 100.
#'
#' All patient-level input vectors must have the same length. If any required
#' patient-level measurement is missing, the corresponding CLIF-C AD score is
#' returned as NA.
#'
#' @references Jalan, R., Pavesi, M., Saliba, F., Amorós, A., Fernandez, J.,
#'   Holland-Fischer, P., Sawhney, R., Mookerjee, R., Caraceni, P., Moreau, R.,
#'   Ginès, P., Durand, F., Angeli, P., Alessandria, C., Laleman, W., Trebicka,
#'   J., Samuel, D., Zeuzem, S., Gustot, T., Gerbes, A. L., Wendon, J.,
#'   Bernardi, M., Arroyo, V., CANONIC study investigators of the EASL-CLIF
#'   Consortium. The CLIF Consortium Acute Decompensation score (CLIF-C ADs) for
#'   prognosis of hospitalised cirrhotic patients without acute-on-chronic liver
#'   failure. J. Hepatol. 2015, 62 (4), 831–840. DOI: 10.1016/j.jhep.2014.11.012
#' 
#' @param age Numeric vector of age in years.
#' @param creatinine Numeric vector of creatinine in mg/dL (default) or
#'   \eqn{\mu mol/L} when unit = "SI".
#' @param inr Numeric vector of international normalized ratio (INR).
#' @param wbc Numeric vector of white blood cell count in \eqn{10^9/L}.
#' @param sodium Numeric vector of serum sodium in mmol/L.
#' @param digits Non-negative integer specifying the number of decimal places
#'   used to round the final score. Default is 0.
#' @param units Character scalar specifying whether creatinine is supplied in
#'   "US" units (mg/dL, default) or "SI" units (\eqn{\mu mol/L}). When
#'   "SI" is selected, creatinine is converted internally to mg/dL before
#'   calculation.
#'
#' @returns Numeric vector of CLIF-C AD scores bounded between 0 and 100 and
#'   rounded to digits decimal places.
#'
#' @export
#'
#' @examples
#' clif_ad(age = 50, creatinine = 2, inr = 2, wbc = 20, sodium = 137, unit = "US")
#' # 69
#' 
#' # Arguments may also be supplied positionally:
#' clif_ad(50, 2, 2, 20, 137)
#' # 69
#'  
#' # Creatinine supplied in SI units is converted internally to mg/dL.
#' clif_ad(age = 50, creatinine = 90, inr = 2, wbc = 20, sodium = 137, unit = "SI")
#' # 65
#' 
#' # The function is fully vectorized:
#' clif_ad(age = c(50, 70, 22), creatinine = c(90, 120, 70),
#'  inr = c(2, 1.8, 2.2), wbc = c(20, 60, 11),
#'   sodium = c(137, 122, 140), unit = "SI")
#'  # 65, 88, 50


clif_ad <- function(age, creatinine, inr,
                    wbc, sodium,
                    digits = 0, units = "US"){
  
  # Check that digits is either default or not provided
  if (!is.numeric(digits) ||
      length(digits) != 1 ||
      is.na(digits) ||
      digits < 0 ||
      digits %% 1 != 0) {
    stop("digits must be a single non-negative integer")
  }
  
  # Input validation
  numeric_inputs <- list(
    age = age,
    creatinine = creatinine,
    inr = inr,
    wbc = wbc,
    sodium = sodium
  )
  
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))){
    stop("Age, creainine, inr, white blood cells and sodium must be numeric")
  }
  
  .check_scalar_choice(units, c("SI", "US"), "unit")
  
  # Input length validation
  input_lengths <- vapply(numeric_inputs, length, integer(1))
  
  if(any(input_lengths == 0L)) {
    stop("Input vectors must contain at least one observation")
  }
  
  if(length(unique(input_lengths)) != 1L) {
    stop("All patient-level input vectors must have the same length")
  }
  
  # Creatinine transformation to mg/dL
  
  if(units == "SI") {
    creatinine <- creatinine * 0.0113
  }
  
  clif_ad <- 10 * (
    0.03 * age +
    0.66 * log(creatinine) +
    1.71 * log(inr) + 
    0.88 * log(wbc) -
    0.05 * sodium + 8)
  
  # Bounded between 0 and 100
  clif_ad <- pmin(pmax(clif_ad, 0), 100)
  round(clif_ad, digits = digits)
}