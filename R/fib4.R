#' Fibrosis-4 (FIB-4) Index for Liver Fibrosis
#'
#' @details
#' Fully vectorized function for calculating the FIB-4 index. Missing
#' patient-level values return `NA` for the corresponding score.
#'
#' FIB-4 was originally developed by Sterling et al. (2006) using age,
#' AST, ALT, and platelet count:
#'
#' \deqn{
#' FIB-4 =
#' \frac{Age \times AST}
#' {platelets \times \sqrt{ALT}}
#' }
#'
#' Interpretation should be made cautiously in younger and older patients.
#' McPherson et al. reported poor performance in patients aged <=35 years
#' and reduced specificity in patients aged >=65 years.
#'
#' @references Sterling RK, Lissen E, Clumeck N, Sola R, Correa MC, Montaner J,
#'   S Sulkowski M, Torriani FJ, Dieterich DT, Thomas DL, Messinger D, Nelson M;
#'   APRICOT Clinical Investigators. Development of a simple noninvasive index
#'   to predict significant fibrosis in patients with HIV/HCV coinfection.
#'   Hepatology. 2006 Jun;43(6):1317-25. doi: 10.1002/hep.21178. PMID: 16729309.
#'
#'   McPherson S, Hardy T, Dufour JF, Petta S, Romero-Gomez M, Allison M,
#'   Oliveira CP, Francque S, Van Gaal L, Schattenberg JM, Tiniakos D, Burt A,
#'   Bugianesi E, Ratziu V, Day CP, Anstee QM. Age as a Confounding Factor for
#'   the Accurate Non-Invasive Diagnosis of Advanced NAFLD Fibrosis. Am J
#'   Gastroenterol. 2017 May;112(5):740-751. doi: 10.1038/ajg.2016.453. Epub
#'   2016 Oct 11. PMID: 27725647; PMCID: PMC5418560.
#'   
#' @seealso [elf()]
#'
#' @param age Numeric vector of the age in years
#' @param asat Numeric vector of aspartate aminotransferase (ASAT) in \eqn{U/L}
#' @param platelets Platelet count measured in either \eqn{10^9/L} or \eqn{10^3/\mu L}
#' @param alat Numeric vector of Alanine aminotransferase (ALAT) in \eqn{U/L}
#' @param digits Single, non-negative integer, deciding the final rounding
#'   (default = 2)
#'
#' @returns Numeric vector of the calculated FIB-4 score
#' @export
#'
#' @examples
#' fib4(age = 45, asat = 45, platelets = 145, alat = 45)
#' # 2.08
#'
#' # The function also works on vectors of several measurements
#' fib4(age = c(45, 35, 65), asat = c(64, 20, 230), platelets = c(120, 80, 86), alat = c(80, 14, 180))
#' # 2.68, 2.34, 12.96

fib4 <- function(age, asat, platelets, alat, digits = 2) {
  
  ## Input validation
  numeric_input <- list(
    age = age,
    asat = asat,
    platelets = platelets,
    alat = alat
  )
  
  if(!all(vapply(numeric_input, .is_numeric_or_na, logical(1)))) {
    stop("Input vectors must be numeric")
  } 
  
  if(!is.numeric(digits) || 
     length(digits) != 1L ||
     is.na(digits) || 
     digits < 0 ||
     digits %% 1 != 0) {
    stop("digits must be a single non-negative integer")
  }
  
  if(any(vapply(numeric_input, function(x) any(x <= 0, na.rm = TRUE),logical(1)))) {
    stop("Input vectors cannot be negative or zero")
  }
  
  ## Input length validation
  input_lengths <- vapply(numeric_input, length, integer(1))
  
  if(any(input_lengths == 0L)) {
    stop("Input vectors must contain at least one observation")}
  
  if (length(unique(input_lengths)) != 1){
    stop("All input vectors must have the same length")}
  
  ## Age warning
  if(any(age >= 65 | age <= 35, na.rm = TRUE)) {
    warning("Use with caution when interpreting results for patients aged \u2265
            35 or \u2265 years, as the score has been shown to be less reliable
            in these age groups")
  }
  
  ## Calculation
  missing <- is.na(age) | 
              is.na(asat) |
              is.na(platelets) |
              is.na(alat)
   
    score <- (age * asat) /
    (platelets * sqrt(alat))
  
  score[missing] <- NA_real_
  
  round(score, digits = digits)
}
