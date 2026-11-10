#' Child-Pugh score
#'
#' Calculates the Child-Pugh score
#'
#' @details
#' The Child-Pugh score is calculated from clinical and biochemical parameters.
#' The biochemical parameters are serum bilirubin, serum albumin, and the
#' International Normalized Ratio (INR). The clinical parameters are the
#' severity of ascites and the grade of hepatic encephalopathy.
#'
#' Calculations are performed using bilirubin in mg/dL and albumin in g/dL.
#' When units = "SI", bilirubin supplied in \eqn{\mu mol/L} and albumin
#' supplied in g/L are converted internally before calculation.
#'
#' Each of the five components is assigned a score from 1 to 3. The total
#' Child-Pugh score is the sum of the five component scores:
#'
#' \deqn{
#' \mathrm{Child\mbox{-}Pugh} =
#' S_{\mathrm{bilirubin}} +
#' S_{\mathrm{albumin}} +
#' S_{\mathrm{INR}} +
#' S_{\mathrm{ascites}} +
#' S_{\mathrm{HE}}
#' }
#'
#' where each component score \eqn{S} takes a value from 1 to 3, resulting in
#' a total score ranging from 5 to 15.
#'
#' Missing required patient-level data result in an NA score for the
#' corresponding observation.
#' This function does not support higher bilirubin thresholds for cholestatic
#' liver diseases as of version 1.0.0.
#'
#' @references Child CG, Turcotte JG. Surgery and portal hypertension. Major
#' Probl Clin Surg. 1964;1:1-85. PMID: 4950264.
#'
#' Pugh RN, Murray-Lyon IM, Dawson JL, Pietroni MC, Williams R. Transection of
#' the oesophagus for bleeding oesophageal varices. Br J Surg. 1973
#' Aug;60(8):646-9. doi: 10.1002/bjs.1800600817. PMID: 4541913.
#'
#' @param bilirubin Numeric vector of serum bilirubin concentrations in mg/dL
#'  when `units` = "US" or \eqn{\mu mol/L} when `units` = "SI".
#' @param albumin Numeric vector of serum albumin concentrations in g/dL when
#'  `units` = "US" or g/L when `units` = "SI". 
#' @param inr Numeric vector of International Normalized Ratio (INR) values.
#' @param ascites Character vector specifying the clinically assessed severity
#'  of ascites. Must be one of 'absent', 'slight' or 'moderate'.
#' @param he Numeric vector specifying hepatic encephalopathy grade according to
#'  the West Haven criteria. Values must range from 0 to 4. 
#' @param units Character scalar specifying the measurement system. Use "US"
#' (default) for bilirubin in mg/dL and albumin in g/dL, or "SI" for bilirubin in
#' \eqn{\mu mol/L} and albumin in g/L. SI measurements are converted internally 
#' before score calculation.
#' @param bilirubin_conversion_factor Numeric conversion factor used to convert
#' bilirubin from \eqn{\mu mol/L} to mg/dL. Default is 0.0584.
#' @param albumin_conversion_factor Numeric conversion factor used to convert
#' albumin from g/L to g/dL. Default is 0.1.
#'
#' @returns Integer vector of Child-Pugh scores ranging from 5 to 15.
#' @export
#'
#' @examples
#' # This function can calculate the Child-Pugh score for your data:
#' cp_score(bilirubin = 2.2, albumin = 3.0,
#'  inr = 2, ascites = "slight", he = 2, units = "US")
#' # 10
#' 
#' # Or using positional arguments:
#' cp_score(2.2, 3.0, 2, "slight", 2, "US")
#' 
#' # If your measurements use the SI system you can toggle that setting with 
#' # 'units'. This will internally convert your bilirubin and albumin before 
#' # calculating the score.
#' cp_score(bilirubin = 40, albumin = 30, inr = 2,
#'  ascites = "slight", he = 2, units = "SI")
#' 
#' 
#' # This function is fully vectorized, so you should be able to apply it to your
#' # entire dataset:
#' cp_score(bilirubin = c(20, 40, 60),
#'          albumin = c(40, 30, 20),
#'          inr = c(1.2, 1.8, 2.5),
#'          ascites = c("absent", "slight", "moderate"),
#'          he = c(0, 2, 4),
#'          units = "SI")
#'# 5 10 15
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
  
  if(!is.character(ascites) && !(is.logical(ascites) && all(is.na(ascites)))) {
    stop("ascites must be a character vector")}
  
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
  
  if(any(!is.na(he) & !he %in% 0:4)) {
    stop("'he' must be an integer between 0 and 4")
  }
  
  ## characters tolower so capitalization does not break calculation
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
  
  missing <- is.na(bilirubin) | 
    is.na(albumin) |
    is.na(inr) |
    is.na(ascites) |
    is.na(he)
  
  .warn_missing(missing)
  
  ## Unit conversion
  if(units == "SI") {
    bilirubin <- bilirubin * bilirubin_conversion_factor
    albumin <- albumin * albumin_conversion_factor
  }
  
  
  ## Score calculation
  bilirubin_score <- rep(NA_integer_, length(bilirubin))
  bilirubin_score[which(bilirubin > 3)] <- 3L
  bilirubin_score[which(bilirubin >= 2 & bilirubin <= 3)] <- 2L
  bilirubin_score[which(bilirubin < 2)] <- 1L
  
  albumin_score <- rep(NA_integer_, length(albumin))
  albumin_score[which(albumin > 3.5)] <- 1L
  albumin_score[which(albumin >= 2.8 & albumin <= 3.5)] <- 2L
  albumin_score[which(albumin < 2.8)] <- 3L
  
  inr_score <- rep(NA_integer_, length(inr))
  inr_score[which(inr > 2.3)] <- 3L
  inr_score[which(inr <= 2.3 & inr >= 1.7)] <- 2L
  inr_score[which(inr < 1.7)] <- 1L
  
  ascites_score <- rep(NA_integer_, length(ascites))
  ascites_score[which(ascites == "absent")] <- 1L
  ascites_score[which(ascites == "slight")] <- 2L
  ascites_score[which(ascites == "moderate")] <- 3L
  
  he_score <- rep(NA_integer_, length(he))
  he_score[which(he == 0)] <- 1L
  he_score[which(he %in% c(1, 2))] <- 2L
  he_score[which(he %in% c(3, 4))] <- 3L
  
  score <- bilirubin_score + albumin_score + inr_score + ascites_score + he_score
  
  score[missing] <- NA_integer_
  
  score
}