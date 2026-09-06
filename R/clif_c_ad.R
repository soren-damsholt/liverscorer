#' Title CLIF-C-AD score calculation
#' @details
#' Additional details...
#' 
#' @references Jalan, R., Pavesi, M., Saliba, F., Amorós, A., Fernandez, J., 
#' Holland-Fischer, P., Sawhney, R., Mookerjee, R., Caraceni, P., Moreau, R.,
#'  Ginès, P., Durand, F., Angeli, P., Alessandria, C., Laleman, W., Trebicka,
#'   J., Samuel, D., Zeuzem, S., Gustot, T., Gerbes, A. L., Wendon, J., Bernardi,
#'    M., Arroyo, V., CANONIC study investigators of the EASL-CLIF Consortium.
#'     The CLIF Consortium Acute Decompensation score (CLIF-C ADs) for prognosis of hospitalised cirrhotic patients without acute-on-chronic liver failure.
#'      J. Hepatol. 2015, 62 (4), 831–840. DOI: 10.1016/j.jhep.2014.11.012
#'
#' @param age Numeric vector of age in years.
#' @param creatinine Numeric vector of creatinine in either µmol/L or mg/dL.
#' @param inr Numeric vector of International normalized ratio (INR)
#' @param wbc Numeric vector of white blood cell count (x 10⁹ cells/L) 
#' @param sodium Numeric vector of sodium (mmol/L)
#' @param digits Option for specifying digits for rounding (default = 0)
#' @param unit Option specifying if creatinine is using SI or US units. If the
#' option i set to SI, creatinine is automatically converted to mg/dL before 
#' calculating the CLIF-C-AD score.
#'
#' @returns Numeric vector of CLIF-C-AD score
#' 
#' @export
#'
#' @examples
#' clif_ad(age = 50, creatinine = 2, inr = 2, wbc = 20, sodium = 137, unit = "US")
#' # 69
#' 
#' # Or simply:
#' clif_ad(50, 2, 2, 20, 137)
#' # 69
#'  
#' # If your creatinine follows SI units, then specify it before calculating:
#' clif_ad(age = 50, creatinine = 90, inr = 2, wbc = 20, sodium = 137, unit = "SI")
#' # 65
#' 
#' # The function is fully vectorized and should handle multiple values:
#' clif_ad(age = c(50, 70, 22), creatinine = c(90, 120, 70),
#'  inr = c(2, 1.8, 2.2), wbc = c(20, 60, 11),
#'   sodium = c(137, 122, 140), unit = "SI")
#'  # 65, 88, 55


clif_ad <- function(age, creatinine, inr,
                    wbc, sodium,
                    digits = 0, unit = "US"){
  
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
  
  .check_scalar_choice(unit, c("SI", "US"), "unit")
  
  # Input length validation
  input_lengths <- vapply(numeric_inputs, length, integer(1))
  
  if(any(input_lengths == 0L)) {
    stop("Input vectors must contain at least one observation")
  }
  
  if(length(unique(input_lengths)) != 1L) {
    stop("All patient-level input vectors must have the same length")
  }
  
  # Creatinine transformation to mg/dL
  
  if(unit == "SI") {
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