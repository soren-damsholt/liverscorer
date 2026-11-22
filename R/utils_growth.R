.interpolate_growth <- function(age_months, reference) {
  
  if(!is.numeric(age_months) || length(age_months) != 1L ||
     is.na(age_months) || age_months < 0) {
    stop("age_months must be as single non-negative value")
  }
  
  if(age_months < min(reference$age_months) ||
     age_months > max(reference$age_months)) {
    stop("age_months is ouside the available growth refrence range")
  }
  
  
  exact <- which(reference$age_months == age_months)
  
  if(length(exact) == 1) {
    return(c(boxcox_lambda = reference$boxcox_lambda[exact],
             median = reference$median[exact],
             cov_variance = reference$cov_variance[exact]))
  }
  
  lower <- max(which(reference$age_months < age_months))
  upper <- min(which(reference$age_months > age_months))
  
  lower_age <- reference$age_months[lower]
  upper_age <- reference$age_months[upper]
  
  fraction <- (age_months - lower_age) /
                (upper_age - lower_age)
  c(
    boxcox_lambda = reference$boxcox_lambda[lower] + fraction * 
      (reference$boxcox_lambda[upper] - reference$boxcox_lambda[lower]),
    
    median = reference$median[lower] + fraction * 
      (reference$median[upper] - reference$median[lower]),
    
    cov_variance = reference$cov_variance[lower] + fraction * 
      (reference$cov_variance[upper] - reference$cov_variance[lower])
  )
  
}


.lms_z_score <- function(measurement, boxcox_lambda, median, cov_variance) {
  
  if(abs(boxcox_lambda) < .Machine$double.eps) {
    return(log(measurement / median) / cov_variance)
  }
  
  ((measurement / median) ^ boxcox_lambda - 1) / (boxcox_lambda * cov_variance)
}
  

.cdc_growth_z <- function(measurement, age_months, sex, reference) {
  
  input_lengths <- c(
    measurement = length(measurement),
    age_months = length(age_months),
    sex = length(sex)
  )
  
  if(any(input_lengths == 0L)) {
    stop("Patient-level inputs must contain at least one observation")
  }
  
  if(length(unique(input_lengths)) != 1L) {
    stop("measurement, age_months, sex must all have the same length")
  }
  
  if(!is.numeric(measurement)) {
    stop("measurement must be numeric")
  }
  
  if(!is.numeric(age_months)) {
    stop("age_months must be numeric")
  }
  
  if(!is.character(sex)) {
    stop("sex must be character")
  }
  
  if(any(!is.na(sex) & !sex %in% c("male", "female"))) {
    stop("sex must be 'male' or 'female'")
  }
  
  n <- length(measurement)
  
  z_score <- rep(NA_real_, n)
  
  for(i in seq_len(n)) {
    
    if(is.na(measurement[i]) ||
       is.na(age_months[[i]]) ||
       is.na(sex[i])) {
      next
    }
    
    sex_reference <- reference[reference$sex == sex[i], ]
    
    growth_refrence <- .interpolate_growth(age_months = age_months[i],
                                           reference = sex_reference)
    
    z_score[i] <- .lms_z_score(measurement = measurement[i],
                               boxcox_lambda = growth_refrence["boxcox_lambda"],
                               median = growth_refrence["median"],
                               cov_variance = growth_refrence["cov_variance"]
                               )
  
  }
  
  z_score
}
  
  
  
  
  
  
  
  
  
  