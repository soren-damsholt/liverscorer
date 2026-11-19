#' AST to Platelet Ratio Index (APRI)
#' 
#' Vectorized function for calculating the APRI score
#' 
#' @details
#' The AST to Platelet Ratio Index (APRI) is calculated from serum aspartate
#' aminotransferase (AST), the laboratory-specific upper limit of normal(ULN)
#' for AST, and the platelet count.
#' 
#' The score is calculated as:
#' \deqn{
#' APRI = \fraq{AST / AST_[ULN]}{platelets}\times 100}
#' 
#' where AST and its ULN are expressed in U/L and platelet count in \eqn{10^9/L}.
#' `ast_uln` may be supplied as a scalar, in which case it is applied to all 
#' observations, or as a vector matching the patient-level input lenght. 
#' Missing inputs will result in `NA` for the corresponding observation.
#' 
#' AST ULN is laboratory specific and should be carefully considered and applied
#' according to your lab standard. Default is 45, but this may vary. 
#' 
#' @references 
#' Wai CT, Greenson JK, Fontana RJ, Kalbfleisch JD, Marrero JA, Conjeevaram HS,
#' Lok AS. A simple noninvasive index can predict both significant fibrosis and
#' cirrhosis in patients with chronic hepatitis C. Hepatology. 2003
#' Aug;38(2):518-26. doi: 10.1053/jhep.2003.50346. PMID: 12883497.
#' 
#'
#' @param ast Numeric vector of serum aspartate aminotransferase concentrations
#'  in U/L.
#' @param ast_uln Numeric scalar or vector specifying the laboratory specific
#'  upper limit of normal for AST in U/L. If scalar, it will be applied to all
#'  observations. Default is `45`.
#' @param platelets Numeric vector of platelet count in \eqn{10^9/L}
#'
#' @returns Numeric vector of calculated APRI scores.
#' @export
#'
#' @examples
#' apri(80, 40, 250)
#' # 0.8
#' 
#' apri(40, 45, 250)
#' apri(ast = 40, platelets = 250)
#' # 0.3555556
#' 
#' apri(c(80, 100), 45, c(250, 250))
#' apri(c(80, 100), c(45, 45), c(250, 250))
#' # 0.7111111 0.8888889
apri <- function(ast, ast_uln = 45, platelets) {
  
  # Input validation
  numeric_inputs <- list(
    ast = ast,
    ast_uln = ast_uln,
    platelets = platelets
  )
  
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))){
    stop("ast, ast_uln and platelets must be numeric")}
  
  # Length validation
  input_lengths <- c(
    ast = length(ast),
    platelets = length(platelets))
  
  if(any(input_lengths == 0L)) {
    stop("Patient-level inputs must contain at least one observation")}
  
  if(length(unique(input_lengths)) != 1) {
    stop("All patient-level input vectors must have the same length")}
  
  n <- unique(input_lengths)
  
  .check_scalar_or_length(ast_uln, n, "ast_uln")
  
  if(length(ast_uln) == 1L) {
    ast_uln <- rep(ast_uln, n)}
  
  
  # Range validation
  if(any(ast < 0, na.rm = TRUE)) {
    stop("ast cannot be negative")}
  
  if(any(ast_uln <= 0, na.rm = TRUE)){
    stop("ast_uln must be greater than 0")}
  
  if(any(platelets <= 0, na.rm = TRUE)){
    stop("platelets must be greater than 0")}
  
  # missing input produces warning
  missing <- is.na(ast) | is.na(ast_uln) | is.na(platelets)
  .warn_missing(missing)
  
  # Calculation
  score <- ast / ast_uln / platelets * 100
  
  score
}