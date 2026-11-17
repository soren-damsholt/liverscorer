#' AST to Platelet Ratio Index (APRI)
#' 
#' Vectorized function for calculating the APRI score
#' 
#' @details
#' 
#' @references 
#' 
#'
#' @param ast Numeric vector of AST (U/L)
#' @param ast_uln Numeric scalar or vector of upper limit of normal for AST
#' @param platelets Numeric vector of platelet count \eqn{10^9/L}
#'
#' @returns Numeric value or vector of calculated APRI score
#' @export
#'
#' @examples
#' apri(80, 40, 250)
#' # 0.8
#' 
#' apri(40, 45, 250)
#' apri(ast = 40, platelets = 250)
#' # 0.35
#' 
#' apri(c(80, 100), 45, c(250, 250))
#' apri(c(80, 100), c(45, 45), c(250, 250))
#' # 0.71 0.88
apri <- function(ast, ast_uln = 45, platelets) {
  
  # Input validation
  numeric_inputs <- list(
    ast = ast,
    ast_uln = ast_uln,
    platelets = platelets
  )
  
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))){
    stop("Input vectors must be numeric")
  }
  
  # Length validation
  input_lengths <- c(
    ast = length(ast),
    platelets = length(platelets)
  )
  
  if(any(input_lengths == 0L)) {
    stop("Patient-level inputs must contain at least one observation")
  }
  
  if(length(unique(input_lengths)) != 1) {
    stop("All patient-level input vectors must have the same length")
  }
  
  n <- unique(input_lengths)
  
  .check_scalar_or_length(ast_uln, n, "ast_uln")
  
  
  # Calculation
  apri <- ast / ast_uln / platelets * 100
  
  apri
  
}