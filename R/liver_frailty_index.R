lfi <- function(grip1, grip2, grip3, time_to_stand,
                side_balance, semi_tandem, tandem, 
                sex, grip_mean = NULL, grip_option = FALSE,
                digits = 2) {
  
  # Input type validation
  
  # Missing values + must define sex as either vector of length nrow or 1?
  
  
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
  
  # Sex adjusted grip strength
    
    if(sex == "male"){
      # z score standardization
      
      sex_adjusted_grip <- (avgGrip - 34.175) / 9.976
    } else if(sex == "female") {
      
      sex_adjusted_grip <- (avgGrip - 21.863) / 6.312
    }
  
  # Liver Frailty Index calculation (rounding???)
  
  lfi <-  (-0.330 * sex_adjusted_grip) +
          (-2.529 * chairstand_per_second) +
          (-0.040 * total_balance_time) + 
          6
  
  round(lfi, digits = digits)
}

