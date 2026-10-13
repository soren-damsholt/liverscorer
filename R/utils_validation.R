.is_numeric_or_na <- function(x){
  is.numeric(x) || (is.logical(x) && all(is.na(x)))
}

.check_range <- function(x, lower = -Inf, upper = Inf, name) {
  if(any(x < lower | x > upper, na.rm = TRUE)){
    stop(
      sprintf("%s must be between %s and %s",
              name, lower, upper)
    )}
}

.check_scalar_choice <- function(x, choices, name) {
  if(length(x) != 1L || is.na(x) || !x %in% choices) {
    stop(
      sprintf("%s must be one of: %s",
              name, paste(choices, collapse = ", "))
    )
  }
}

.check_logical <- function(x, name, allow_na = FALSE) {
  if(!is.logical(x)){
    stop(sprintf("%s must be TRUE or FALSE",
                 name))
    }
  if(!allow_na && anyNA(x)){
    stop(sprintf("%s must be TRUE or FALSE",
                 name))
  }
}

.check_scalar_or_length <- function(x, n, name){
  if(!length(x) %in% c(1L, n)){
    stop(sprintf("%s must have length 1 or match the patient-level input length",
                 name))
  }
}
