#' CIRrhotic Ascites Severity (CIRAS) score
#' 
#' Calculates the CIRrhotic Ascites Severity (CIRAS) score.
#' @details
#' The CIRrhotic Ascites Severity (CIRAS) model combines patient-reported ascites
#' discomfort, clinically assessed ascites accumulation, use of diuretics, and 
#' plasma sodium to calculate an integer score. 
#' CIRAS scores range from 0 to 6, with higher scores reflecting more severe
#' ascites-related disease burden.
#' 
#' Ascites discomfort is assessed using a visual analogue scale from 0 to 100.
#' Ascites accumulation is classified by clinical examination for presence/absence
#' of ascites. If present, use of diuretics is used to stratify patients into two
#' ascites groups.
#' Plasma sodium is categorized according to the thresholds defined in the
#' origial CIRAS model.
#' 
#' Missing required data will result in an NA score for the corresponding
#' observation.
#' 
#' @references 
#' Gantzel RH, Aagaard NK, Vilstrup H, Watson H, Grønbæk H, Jepsen P.
#' Development and validation of the Cirrhotic Ascites Severity model-A
#' patient-reported outcome-based model to predict 1-year mortality. Hepatol
#' Commun. 2022 Nov;6(11):3175-3185. doi: 10.1002/hep4.2065. Epub 2022 Aug 16.
#' PMID: 35972345; PMCID: PMC9592767. 
#' 
#' Gantzel RH, Deleuran T, Vilstrup H, Watson H, Jepsen P. The CIRrhotic Ascites
#' Severity (CIRAS) model predicts hepatic hydrothorax at all stages of ascites.
#' Clin Res Hepatol Gastroenterol. 2024 Oct;48(8):102452. doi:
#' 10.1016/j.clinre.2024.102452. Epub 2024 Aug 22. PMID: 39179203.
#'
#' @param ascites_discomfort Numeric vector of patient-reported ascites 
#' discomfort on a visual analogue scale from 0 to 100.
#' @param ascites_accumulation Logical vector indicating whether ascites 
#' accumulation is present. 
#' @param diuretics Logical vector indicating whether the patient is receiving
#' diuretic treatment at the time of assessment.
#' @param sodium Numeric vector of plasma sodium concentrations measured in mmol/L.
#'
#' @returns Integer vector of CIRAS score.
#' @export
#'
#' @examples
#' ciras(50, ascites_accumulation = TRUE, diuretics = FALSE, sodium = 140)
#' # 2
ciras <- function(ascites_discomfort,
                  ascites_accumulation,
                  diuretics,
                  sodium) {
  
  # Input validation
  numeric_inputs <- list(
    ascites_discomfort = ascites_discomfort,
    sodium = sodium
  )
  if(!all(vapply(numeric_inputs, .is_numeric_or_na, logical(1L)))){
    stop("sodium and ascites_discomfort must be numeric")
  }
  
  if(any(ascites_discomfort < 0 | ascites_discomfort > 100, na.rm = TRUE)) {
    stop("ascites_discomfort must be between 0 and 100")
  }
  
  if(any(sodium <= 0, na.rm = TRUE)) {
    stop("sodium must be greater than 0")
  }
  
  .check_logical(ascites_accumulation, "ascites_accumulation", allow_na = TRUE)
  .check_logical(diuretics, "diuretics", allow_na = TRUE)
  
  # Length validation:
  input_lengths <- c(
    ascites_discomfort = length(ascites_discomfort),
    ascites_accumulation = length(ascites_accumulation),
    diuretics = length(diuretics),
    sodium = length(sodium)
  )
  if(any(input_lengths == 0L)) {
    stop("Patient-level inputs must contain at least one observation")
  }
  
  if(length(unique(input_lengths)) != 1L) {
    stop("All patient-level input vectors must have the same length")
  }
  
  n <- input_lengths[[1L]]
  
  # grouping
  discomfort_group <- rep(NA_character_, n)
  discomfort_group[which(ascites_discomfort <= 50)] <- "low"
  discomfort_group[which(ascites_discomfort > 50)] <- "high"
  
  ascites_group <- rep(NA_character_, n)
  ascites_group[which(!ascites_accumulation)] <- "no_accumulation"
  ascites_group[which(ascites_accumulation & diuretics)] <-"accumulation_diuretics"
  ascites_group[which(ascites_accumulation & !diuretics)] <- "accumulation_no_diuretics"
  
  natrium_group <- rep(NA_character_, n)
  natrium_group[which(sodium >= 140)] <- "ge140"
  natrium_group[which(sodium >= 133 & sodium < 140)] <- "133_139"
  natrium_group[which(sodium >= 125 & sodium < 133)] <- "125_132"
  natrium_group[which(sodium < 125)] <- "lt125"
  
  
  # Missing
  missing  <- is.na(discomfort_group) |
              is.na(ascites_group) |
              is.na(natrium_group)
  
  .warn_missing(missing)
  
  #Lookup
  patient_key <- paste(discomfort_group, ascites_group, natrium_group,
                       sep = "|")
  lookup_key <- paste(.ciras_lookup$ascites_discomfort,
                      .ciras_lookup$ascites,
                      .ciras_lookup$natrium,
                      sep = "|")
  
  score <- .ciras_lookup$score[
    match(patient_key, lookup_key)]
  
  if(any(is.na(score) & !missing)) {
    stop("Internal CIRAS lookup failed for a valid input combination")
  }
  
  score
}


.ciras_lookup <- data.frame(
  ascites_discomfort = rep(
    c("low", "low", "low",
    "high", "high", "high"),
    times = 4),
  
  ascites = rep(c(
    "no_accumulation",
    "accumulation_diuretics",
    "accumulation_no_diuretics",
    "no_accumulation",
    "accumulation_diuretics",
    "accumulation_no_diuretics"),
    times = 4)
  , 
  natrium = rep(
    c("ge140","133_139", "125_132", "lt125"),
    each = 6
  ),
  score = c(0, 1, 2,
            1, 2, 3,
            1, 2, 3,
            2, 3, 4,
            2, 3, 4,
            3, 4, 5, 
            4, 5, 6,
            5, 5, 6),
  stringsAsFactors = FALSE
)