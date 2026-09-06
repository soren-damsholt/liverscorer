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

