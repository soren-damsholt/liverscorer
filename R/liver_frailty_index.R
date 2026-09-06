#' Title Liver Frailty Index
#' Vectorized function for calculating the Liver Frailty Index (LFI) score
#' @details
#' The Liver Frailty Index (LFI) is calculated from three performance-based
#' measures: grip strength, time to complete five chair stands, and balance
#' time across three standing positions. Grip strength is measured in the
#' dominant hand over three attempts and averaged. Each balance position is
#' measured for a maximum of 10 seconds.
#'
#' Grip strength is sex-adjusted using the values used by the UCSF Liver
#' Frailty Index calculator:
#' \enumerate{
#'   \item Male:
#'   \deqn{\frac{avg\_grip - 34.175}{9.976}}
#'   \item Female:
#'   \deqn{\frac{avg\_grip - 21.863}{6.312}}
#' }
#'
#' The LFI is then calculated following the formula provided by Lai et al. (2017):
#' \deqn{
#' LFI =
#' -0.330 \times sex\text{-}adjusted\ grip +
#' -2.529 \times chair\ stands\ per\ second +
#' -0.040 \times balance\ time + 6
#' }
#' 
#' Each balance measurement must be between 0 and 10 seconds. If the patient cannot complete all five chair stands within 60 seconds,
#' 'time_to_stand' should be entered as 0.
#' 
#' Missing patient-level inputs return 'NA' for the corresponding LFI score.
#' Patient-level input vectors must all have the same length; scalar recycling of clinical measurements is not performed.
#' 
#' @references Lai JC, Covinsky KE, Dodge JL, Boscardin WJ, Segev DL, Roberts JP, Feng S. Development of a novel frailty index to predict mortality in patients with end-stage liver disease. Hepatology. 2017 Aug;66(2):564-574. doi: 10.1002/hep.29219. Epub 2017 Jun 28. PMID: 28422306; PMCID: PMC5519430.
#'
#' @param grip1 Numeric vector of first grip measurement in kg.
#' @param grip2 Numeric vector of second grip measurement in kg.
#' @param grip3 Numeric vector of third grip measurement in kg.
#' @param time_to_stand Numeric vector of seconds taken to do 5 chair stands.
#' @param side_balance Numeric vector of seconds maintaining the side-by-side
#'   stance, from 0 to 10 seconds.
#' @param semi_tandem Numeric vector of seconds maintaining the semi-tandem
#'   stance, from 0 to 10 seconds.
#' @param tandem Numeric vector of seconds maintaining the tandem stance,
#'   from 0 to 10 seconds.
#' @param sex Character vector specifying either 'male' or 'female' for grip calculation.
#' @param grip_mean Numeric vector of precalculated mean grip strength in kg.
#'   Used only when `grip_option = TRUE`.
#' @param grip_option Logical scalar. If `FALSE` (default), `grip1`, `grip2`,
#'   and `grip3` are averaged. If `TRUE`, `grip_mean` is used instead.
#' @param digits Non-negative integer specifying the number of decimal places
#'   used to round the returned LFI score. Default is 2.
#'
#' @returns Numeric vector of the liver frailty index calculated to the specified number of digits.
#' @export 
#'
#' @examples
#' lfi(grip1 = 25, grip2 = 26, grip3 = 27, time_to_stand = 10, side_balance = 10, semi_tandem = 10, tandem = 10, sex = "male")
#' # 3.81
#' 
#' # Or simply:
#' lfi(25, 26, 27, 10, 10, 10, 10, sex = "male")
#' # 3.81
#' 
#' # If you have precalculated your mean this is also an option:
#' lfi(grip_mean = 26, time_to_stand = 10, side_balance = 10, semi_tandem = 10, tandem = 10, sex = "male", grip_option = TRUE)
#' # 3.81
#' 
#' # This function is also vectorized:
#' lfi(c(25, 30, 45), c(26, 28, 42), c(27, 32, 49), c(10, 8.3, 6), c(10, 10, 10), c(10, 10, 10), c(6, 9.5, 10), sex = c("male", "female", "male"))
#' # 3.97 2.87 2.32
#' 
lfi <- function(grip1 = NULL, grip2 = NULL, grip3 = NULL, time_to_stand,
                side_balance, semi_tandem, tandem, 
                sex, grip_mean = NULL, grip_option = FALSE,
                digits = 2) {
  
  # Input type validation
    # Check if the grip option i set correctly  
    if (!is.logical(grip_option) ||
        length(grip_option) != 1 ||
        is.na(grip_option)) {
      stop("grip_option must be TRUE or FALSE")
    }
  
    # Check that digits is either default or not provided
      if (!is.numeric(digits) ||
          length(digits) != 1 ||
          is.na(digits) ||
          digits < 0 ||
          digits %% 1 != 0) {
        stop("digits must be a single non-negative integer")
      }
  
    # Set lists of numeric inputs based on grip_option
    if(!grip_option){
      numeric_inputs <- list(
        grip1 = grip1,
        grip2 = grip2,
        grip3 = grip3,
        time_to_stand = time_to_stand,
        side_balance = side_balance,
        semi_tandem = semi_tandem,
        tandem = tandem
      )
    } else {
      numeric_inputs <- list(
        grip_mean = grip_mean,
        time_to_stand = time_to_stand,
        side_balance = side_balance,
        semi_tandem = semi_tandem,
        tandem = tandem)
      }
    
    # Numeric validation, allowing for missing values:
    if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))){
      stop("Grip, balance and chair-stand inputs must be numeric")
    }
  
    # Check if SEX is coded as either "male" or "female"
    if(!all(sex %in% c("male", "female", NA))){
      stop("'sex' must be coded as either 'male' or 'female'")
    }

    # Vector length validation
    patient_inputs <- c(
      numeric_inputs,
      list(sex = sex)
    )
    
    input_lengths <- vapply(patient_inputs, length, integer(1))
    
    if(any(input_lengths == 0L)) {
      stop("Input vectors must contain at least one observation")
    }
    
    if(length(unique(input_lengths)) != 1L) {
      stop("All patient-level input vectors must have the same length")
    }
    
    # Check if each balance value has a value between 0 and 10
    .check_range(side_balance, 0, 10, "side_balance")
    .check_range(semi_tandem, 0, 10, "semi_tandem")
    .check_range(tandem, 0, 10, "tandem")
    
    # Check if the range of time to stand is between 0, 60
    .check_range(time_to_stand, 0, 60, "time_to_stand")
  
  # Calculation:
    # Chairstands should be parsed to chairstand per second
    
       chairstand_per_second <- ifelse(time_to_stand == 0,
                                       0,
                                       5 / time_to_stand)
    # total balance time:
      total_balance_time <- side_balance + semi_tandem + tandem
  
    # Mean grip strength
       if(!grip_option){
         avg_grip <- (grip1 + grip2 + grip3) / 3
       } else {
         avg_grip <- grip_mean
       }
       
       if(any(avg_grip < 0, na.rm = TRUE)){
         stop("Grip strength cannot be negative")
       }
  
   # Missing values?
     missing <- is.na(avg_grip) |
       is.na(time_to_stand) |
       is.na(side_balance) |
       is.na(semi_tandem) |
       is.na(tandem) |
       is.na(sex)
     
  # Sex adjusted grip strength
     sex_adjusted_grip <- ifelse(sex == "male",
                                (avg_grip - 34.175) / 9.976,
                                (avg_grip - 21.863) / 6.312)
     
  # Liver Frailty Index calculation
  lfi_score <-  (-0.330 * sex_adjusted_grip) +
          (-2.529 * chairstand_per_second) +
          (-0.040 * total_balance_time) + 
          6
  
  
  lfi_score[missing] <- NA_real_
  
  round(lfi_score, digits = digits)
}

