lfi <- function(grip1 = NULL, grip2 = NULL, grip3 = NULL, time_to_stand,
                side_balance, semi_tandem, tandem, 
                sex, grip_mean = NULL, grip_option = FALSE,
                digits = 2) {
  
  # Input type validation
    # Check if input variables are numeric
  
    if(grip_option == FALSE & any(!is.numeric(grip1) | !is.numeric(grip2) | !is.numeric(grip3) |
           !is.numeric(time_to_stand) | !is.numeric(side_balance) | !is.numeric(semi_tandem) |
           !is.numeric(tandem))) {
      stop("Input variables are not numeric, please check datatype of grip, balance and chairstand")
    } else if(
      grip_option == TRUE & any(!is.numeric(grip_mean) | !is.numeric(time_to_stand) | 
                                !is.numeric(side_balance) | !is.numeric(semi_tandem) |
                                 !is.numeric(tandem))){
      stop("Input variables are not numeric, please check datatype of grip, balance and chairstand")
      
    }
    # Check if SEX is coded as either "male" or "female"
    if(!all(sex %in% c("male", "female"))){
      stop("Sex must be coded as either 'male' or 'female")
    }
  
  # Missing values <-- needs testing still after vectorization works.
  
    missing <- is.na(grip1) & is.na(grip2) & is.na(grip3) & is.na(grip_mean) |
                is.na(time_to_stand) |
                is.na(side_balance) |
                is.na(semi_tandem) |
                is.na(tandem) |
                is.na(sex)
                
  
  # must define sex as either vector of length nrow or 1?
  
    
  
  
  # Cutoffs? 
  # grip strength is cut-off between 0 and 90 kg, either warning or manually set cap.
  # rss should be between 0 - 60 seconds. If higher set to 0.
  
  
  # Chairstands should be parsed to chairstand per second
  
     chairstand_per_second <- 5 / time_to_stand
  
  
  # total balance time:
     total_balance_time <- side_balance + semi_tandem + tandem
  
  # Mean grip strength
     if(grip_option == FALSE){
       avgGrip <- (grip1 + grip2 + grip3) / 3
     } else{
       
       avgGrip <- grip_mean
       
     }
  
  # Sex adjusted grip strength <- now vectorized, but should probably be rewritten without ifelse
     sex_adjusted_grip <- ifelse(sex == "male",
                                (avgGrip - 34.175) / 9.976,
                                (avgGrip - 21.863) / 6.312)

    
    
  # Liver Frailty Index calculation (rounding???)
  
  lfi <-  (-0.330 * sex_adjusted_grip) +
          (-2.529 * chairstand_per_second) +
          (-0.040 * total_balance_time) + 
          6
  
  
  lfi[missing] <- NA_real_
  
  round(lfi, digits = digits)
}

