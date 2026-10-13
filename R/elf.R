#' Enhanced Liver Fibrosis (ELF) score
#'
#' @details This function calculates the ELF score based on direct measurements
#'   of three biochemical markers of extracellular matrix turnover:
#'   Hyaluronic acid, Procollagen III N-terminal Peptide and Tissue Inhibitor of
#'   Metalloproteinase-1. Each biomarker should be in ng/mL. You should set your
#'   analysis platform as either centaur_cp or centaur_xp, depending on how you
#'   analyzed your samples. This setting will change the model coefficients to
#'   match the published formulas. Default (Centaur XP) uses this formula:
#' \enumerate{
#'   \item Centaur XP:
#'   \deqn{ELF = 2.278 + 0.851 \times \log(HA) + 0.751 \times \log(PIIINP) + 0.394 \times \log(TIMP\text{-}1)}
#'   \item Centaur CP:
#'   \deqn{ELF = 2.494 + 0.846 \times \log(HA) + 0.735 \times \log(PIIINP) + 0.391 \times \log(TIMP\text{-}1)}
#' }
#'  
#'  According with the references, the formulas uses the natural logarithm ln.
#'  The function automatically handles missing input values, and returns them as
#'  NA values. Be sure your data is formatted correctly before calculating.
#'  
#' @references reference
#'
#' @seealso [fib4()]
#'
#' @param HA Numeric value/vector of Hyaluronic Acid (ng/mL)
#' @param PIIINP Numeric value/vector of Procollagen III N-terminal Peptide
#'   (ng/mL)
#' @param TIMP Numeric value/vector of Tissue Inhibitor of Metalloproteinase-1
#'   (ng/mL)
#' @param digits single non-negative integer setting the final rounding (default
#'   = 2)
#' @param platform character scalar specifying theanalysis platform. 
#' Options: "centaur_xp" (default) and "centaur_cp". 
#'
#' @returns Numeric vector of ELF score
#' @export
#'
#' @examples
#' ## Input vectors also calculate the score and returns a numeric vector of results.
#' elf(HA = c(7, 8, 10), PIIINP = c(8, 8, 12), TIMP = c(9, 35, 29), platform = "centaur_xp")
#' # 6.36 7.01 7.43
#' 
elf <- function(HA, PIIINP, TIMP, digits = 2, platform = "centaur_xp"){
  
  ## Input validation
  numeric_inputs <- list(
    HA = HA,
    PIIINP = PIIINP,
    TIMP = TIMP)
  
  
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))){
    stop("HA, PIIINP and TIMP must all be numeric")}
  
  if(!is.numeric(digits) || 
     length(digits) != 1L ||
     is.na(digits) || 
     digits < 0 ||
     digits %% 1 != 0) {
    stop("digits must be a single non-negative integer")
  }
  
  ## scalar validation
  .check_scalar_choice(platform, c("centaur_xp", "centaur_cp"), "platform")
  
  if(any(HA <= 0, na.rm = TRUE) ||
     any(PIIINP <= 0, na.rm = TRUE) ||
     any(TIMP <= 0, na.rm = TRUE)) {
    stop("HA, PIIINP and TIMP must all be greater than 0")
  }
  
  ## Missing values
  missing <- is.na(HA) | is.na(PIIINP) | is.na(TIMP)
  
  # Vector length validation (i.e. require that each vector has exactly same length).
  # Except platform which may be a scalar setting.
  input_lengths <- c(length(HA), 
                     length(PIIINP), 
                     length(TIMP))
  
  n <- input_lengths[1]
  
  if(n == 0) {
    stop("Input vectors must contain at least one observation")}
  
  if (length(unique(input_lengths)) != 1){
    stop("All input vectors must have the same length")}
  
  
  
 
  ## platform setting
  coefficients <- switch(
                    platform,
                    centaur_xp = c(intercept = 2.278,  ha = 0.851,
                                   piiinp = 0.751, timp = 0.394),
                    centaur_cp = c(intercept = 2.494, ha = 0.846,
                                   piiinp = 0.735, timp = 0.391))
  
  ## Calculate ELF-score
  
  score <- coefficients[["intercept"]] +
    coefficients[["ha"]] * log(HA) +
    coefficients[["piiinp"]] * log(PIIINP) +
    coefficients[["timp"]] * log(TIMP)
    
  
  
  score[missing] <- NA_real_
  
  round(score, digits = digits)
}
