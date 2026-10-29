cp_score <- function(bilirubin, albumin, inr, ascites, he,
                     units = "US",
                     bilirubin_conversion_factor = 0.0584,
                     albumin_conversion_factor = 0.1) {
  
  ## input validation
  .check_scalar_choice(units, c("US", "SI"), "units")
  
  numeric_inputs <- list(
    bilirubin = bilirubin,
    albumin = albumin,
    inr = inr,
    he = he)
  
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1)))) {
    stop("Bilirubin, albumin, inr and 'he' must be numeric")
  }
  
  if(any(!is.na(he) & !he %in% rep(0:4))) {
    stop("'he' must have a value between 0 and 4")
  }
  
  if(!is.character(ascites) && !(is.logical(ascites) && all(is.na(ascites)))) {
    stop("ascites must be a character vector")}
  
  correct_ascites_input <- c("absent", "slight", "moderate")
  ascites <- tolower(ascites)
  
  if(any(!is.na(ascites) & !ascites %in% correct_ascites_input)) {
    stop("'ascites' must be one of 'absent', 'slight' or 'moderate'")
  }
  
  if(any(bilirubin <= 0, na.rm = TRUE) ||
     any(albumin <= 0, na.rm = TRUE) ||
     any(inr <= 0, na.rm = TRUE)) {
    stop("Lab values must all be > 0 or NA")
  }
  
  ## Input length validation
  
  input_lengths <-c(
    vapply(numeric_inputs, length, integer(1)),
    length(ascites)
  )
  
  if(any(input_lengths == 0L)) {
    stop("All inputs must contain at least one observation")
  }
  
  if(length(unique(input_lengths)) != 1L) {
    stop("All patient-level input vectors must be the same length")
  }
  
  ## characters tolower so capitalization does not break calculation
  
  ## Unit conversion
  if(units == "SI") {
    bilirubin <- bilirubin * bilirubin_conversion_factor
    albumin <- albumin * albumin_conversion_factor
  }
  
  
  ## Score calculation
  bilirubin_score <- rep(NA_real_, length(bilirubin))
  bilirubin_score[bilirubin > 3] <- 3
  bilirubin_score[bilirubin >= 2 & bilirubin <= 3] <- 2
  bilirubin_score[bilirubin < 2] <- 1
  
  albumin_score <- rep(NA_real_, length(albumin))
  albumin_score[albumin > 3.5] <- 1
  albumin_score[albumin >= 2.8 & albumin <= 3.5] <- 2
  albumin_score[albumin < 2.8] <- 3
  
  inr_score <- rep(NA_real_, length(inr))
  inr_score[inr > 2.3] <- 3
  inr_score[inr <= 2.3 & inr >= 1.7] <- 2
  inr_score[inr < 1.7] <- 1
  
  ascites_score <- rep(NA_real_, length(ascites))
  ascites_score[ascites == "absent"] <- 1
  ascites_score[ascites == "slight"] <- 2
  ascites_score[ascites == "moderate"] <- 3
  
  he_score <- rep(NA_real_, length(he))
  he_score[he == 0] <- 1
  he_score[he %in% c(1, 2)] <- 2
  he_score[he %in% c(3, 4)] <- 3
  
  score <- bilirubin_score + albumin_score + inr_score + ascites_score + he_score
  
  score
}



# CITATIONs:
# ORIGINAL: Child CG, Turcotte JG. Surgery and portal hypertension. Major Probl Clin Surg. 1964;1:1-85. PMID: 4950264.
# VALIDATION: Pugh RN, Murray-Lyon IM, Dawson JL, Pietroni MC, Williams R. Transection of the oesophagus for bleeding oesophageal varices. Br J Surg. 1973 Aug;60(8):646-9. doi: 10.1002/bjs.1800600817. PMID: 4541913.