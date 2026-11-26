#' Leipzig score
#'
#' Calculates the Leipzig diagnostic score for Wilson's disease.
#'
#' @details
#'  The Leipzig score combines clinical, biochemical, histological and genetic,
#'  findings to estimate the likelihood of Wilson's disease. The implementation
#'  follows the diagnostic scoring criteria presented in the 2025 EASL-ERN
#'  Clinical Practice Guidelines on Wilson's disease.
#'
#'  Individual components contribute between -1 and 4 points to the total score.
#'  Accoording to the EASL-ERN criteria, a total score of 0-1 indicates that
#'  Wilson's disease is unlikely, a score of 2-3 indicates that is is probable,
#'  and a score of 4 or greater indicates that it is highly likely.
#'
#'  Because some components of the Leipzig score may be unavailbale in clinical
#'  practice, missing inputs are assigned 0 points rather than causing the total
#'  score to be returned as NA. A warning is issued when missing values are
#'  detected.
#'
#'  Rhodanine-positive hepatocytes contribute to the score only when quantitative
#'  liver copper measurement is unavailbale. In this implementation,
#'  `liver_copper = 0 or NA` is treated as an unavailable or unquantifiable liver
#'  cipper measurement for this purpose.
#'
#'  Urinary copper is scored relative to the supplied laboratory-specific upper
#'  limit of normal (ULN). This allows observations from laboratories with
#'  different refrence ranges to be included in the same dataset.
#'
#'  The D-penicillamine challenge criterion is applied only when baseline 24-H
#'  urinary copper is below 1 x ULN. A post-challenge urinary copper of at least
#'  5 x ULN contributes 2 points.
#'
#' @references European Association for the Study of the Liver. EASL-ERN
#'   Clinical Practice Guidelines on Wilson's disease. J Hepatol. 2025 Feb
#'   22:S0168-8278(24)02706-5. doi: 10.1016/j.jhep.2024.11.007. Epub ahead of
#'   print. PMID: 40089450.
#'
#' @param kayser_fleischer Logical vector indicating the presence of
#'   Kayser-Fleischer rings.
#' @param neurological_symptoms Character vector specifying the severity of
#'   neurological symptoms. Must be one of `absent`, `mild` or `severe`.
#' @param ceruloplasmin Numeric vector of serum ceruloplasmin concentrations in
#'   g/L.
#' @param haemolytic_anemia Logical scalar or vector indicating the presence of
#'   Coombs-negative haemolytic anemia. If scalar, the value is applied to all
#'   observations
#' @param liver_copper Numeric vector of quantitative hepatic copper
#'   measurements, expressed relative to the scoring threshold used by the
#'   Leipzig criteria. A value of 0 is treated as unavailable or unquantifiable
#'   for the purpose of rhodanine scoring.
#' @param rhodanine_granules Logical scalar or vector indicating the presence of
#'   Rhodanine-positive hepatocytes on liver biopsy. This criterion is only used
#'   when quantitative liver copper is unavailable.
#' @param urinary_copper Numeric vector of 24-hour urinary copper measurements.
#' @param urinary_copper_uln Numeric scalar or vector giving the
#'   laboratory-specific upper limit of normal for 24-hour urinary copper.
#' @param penicillamine_copper Optional numeric scalar or vector of 24-hour
#'   urinary copper measured after D-penicillamine challenge. If scalar, the
#'   value is applied to all observations. Default is NULL
#' @param mutated_chromosomes Integer vector specifying the number of
#'   chromosomes with detected disease-causing mutations. Must be `0, 1 or 2`
#'
#' @returns Integer vector containing the Leipzig score.
#' @export
#'
#' @examples
#' leipzig(
#'   kayser_fleischer = TRUE,
#'   neurological_symptoms = "absent",
#'   ceruloplasmin = 0.25,
#'   haemolytic_anemia = FALSE,
#'   liver_copper = 0.5,
#'   rhodanine_granules = FALSE,
#'   urinary_copper = 1,
#'   urinary_copper_uln = 1,
#'   penicillamine_copper = NULL,
#'   mutated_chromosomes = 0
#' )
#' # 2
#'
#' leipzig(
#'   kayser_fleischer = c(TRUE, FALSE, TRUE),
#'   neurological_symptoms = c("absent", "mild", "severe"),
#'   ceruloplasmin = c(0.25, 0.5, 1.5),
#'   haemolytic_anemia = c(FALSE, TRUE, FALSE),
#'   liver_copper = c(0.5, 2, 5),
#'   rhodanine_granules = c(FALSE, TRUE, FALSE),
#'   urinary_copper = c(0.6, 1.5, 5),
#'   urinary_copper_uln = c(0.5, 1, 0.5),
#'   penicillamine_copper = c(0.5, 1, 0.5),
#'   mutated_chromosomes = c(0, 1, 2)
#' )
#'
#' # 2, 5, 12
leipzig <- function(kayser_fleischer, neurological_symptoms,
                    ceruloplasmin, haemolytic_anemia = FALSE,
                    liver_copper, rhodanine_granules = FALSE,
                    urinary_copper, urinary_copper_uln,
                    penicillamine_copper = NULL,
                    mutated_chromosomes) {
  # Input validation
  if (!is.character(neurological_symptoms) &&
      !(is.logical(neurological_symptoms) &&
        all(is.na(neurological_symptoms)))) {
    stop("neurological_symptoms must be a character vector")
  }

  neurological_symptoms <- tolower(neurological_symptoms)
  valid_neuro <- c("absent", "mild", "severe")

  if (any(!is.na(neurological_symptoms) &
          !neurological_symptoms %in% valid_neuro)) {
    stop("neurological_symptoms must be one of 'absent', 'mild' or 'severe'")
  }

  ## Numerics
  numeric_input <- list(
    ceruloplasmin = ceruloplasmin,
    liver_copper = liver_copper,
    urinary_copper = urinary_copper,
    mutated_chromosomes = mutated_chromosomes,
    urinary_copper_uln = urinary_copper_uln
  )

  if (!all(vapply(numeric_input, .is_numeric_or_na, logical(1)))) {
    stop("Input vectors must be numeric")
  }

  ## Logical
  .check_logical(kayser_fleischer, "kayser_fleischer", allow_na = TRUE)
  .check_logical(haemolytic_anemia, "haemolytic_anemia", allow_na = TRUE)
  .check_logical(rhodanine_granules, "rhodanine_granules", allow_na = TRUE)

  # length validation
  input_lengths <- c(
    kayser_fleischer = length(kayser_fleischer),
    neurological_symptoms = length(neurological_symptoms),
    ceruloplasmin = length(ceruloplasmin),
    liver_copper = length(liver_copper),
    urinary_copper = length(urinary_copper),
    mutated_chromosomes = length(mutated_chromosomes)
  )

  if (any(input_lengths == 0L)) {
    stop("Patient-level inputs must contain at least one observation")
  }

  if (length(unique(input_lengths)) != 1) {
    stop("All patient-level input vectors must have the same length")
  }

  n <- unique(input_lengths)

  .check_scalar_or_length(haemolytic_anemia, n, "haemolytic_anemia")
  .check_scalar_or_length(rhodanine_granules, n, "rhodanine_granules")
  .check_scalar_or_length(urinary_copper_uln, n, "urinary_copper_uln")

  if (length(haemolytic_anemia) == 1L) {
    haemolytic_anemia <- rep(haemolytic_anemia, n)
  }

  if (length(rhodanine_granules) == 1L) {
    rhodanine_granules <- rep(rhodanine_granules, n)
  }

  if (length(urinary_copper_uln) == 1L) {
    urinary_copper_uln <- rep(urinary_copper_uln, n)
  }

  if (!is.null(penicillamine_copper)) {
    if (!.is_numeric_or_na(penicillamine_copper)) {
      stop("penicillamine_copper must be numeric or NULL")
    }

    .check_scalar_or_length(penicillamine_copper, n, "penicillamine_copper")

    if (length(penicillamine_copper) == 1L) {
      penicillamine_copper <- rep(penicillamine_copper, n)
    }
  }

  if (any(!is.na(mutated_chromosomes) &
          !mutated_chromosomes %in% 0:2)) {
    stop("mutated_chromosomes must be 0, 1 or 2")
  }

  # Stop non-physiological values that break the function:
  if (any(ceruloplasmin < 0, na.rm = TRUE)) {
    stop("ceruloplasmin cannot be negative")
  }

  if (any(liver_copper < 0, na.rm = TRUE)) {
    stop("liver_copper cannot be negative")
  }

  if (any(urinary_copper < 0, na.rm = TRUE)) {
    stop("urinary_copper cannot be negative")
  }

  if (any(urinary_copper_uln <= 0, na.rm = TRUE)) {
    stop("urinary_copper_uln cannot be negative or zero")
  }

  if (!is.null(penicillamine_copper) &&
      any(penicillamine_copper < 0, na.rm = TRUE)) {
    stop("penicillamine_copper cannot be negative")
  }

  # Missing + warning
  missing <- is.na(kayser_fleischer) | is.na(neurological_symptoms) |
    is.na(ceruloplasmin) | is.na(haemolytic_anemia) |
    is.na(liver_copper) | is.na(rhodanine_granules) |
    is.na(urinary_copper) | is.na(urinary_copper_uln) |
    is.na(mutated_chromosomes)

  if (!is.null(penicillamine_copper)) {
    missing <- missing | is.na(penicillamine_copper)
  }

  if (any(missing)) {
    warning("Missing inputs may affect component scoring; unspecified components default to 0")
  }


  # SCORE CALCULATION:
  # Kayser-Fleischer score
  kf_score <- rep(0L, length(kayser_fleischer))
  kf_score[which(kayser_fleischer)] <- 2L
  kf_score[which(!kayser_fleischer)] <- 0L

  # Neurological symptoms:
  neuro_score <- rep(0L, length(neurological_symptoms))
  neuro_score[which(neurological_symptoms == "severe")] <- 2L
  neuro_score[which(neurological_symptoms == "mild")] <- 1L
  neuro_score[which(neurological_symptoms == "absent")] <- 0L

  # Ceruloplasmin
  cerulo_score <- rep(0L, length(ceruloplasmin))
  cerulo_score[which(ceruloplasmin > 0.2)] <- 0L
  cerulo_score[which(ceruloplasmin <= 0.2 & ceruloplasmin >= 0.1)] <- 1L
  cerulo_score[which(ceruloplasmin < 0.1)] <- 2L

  # Coombs negative haemolytic anemia
  anemia_score <- rep(0L, length(haemolytic_anemia))
  anemia_score[which(haemolytic_anemia)] <- 1L
  anemia_score[which(!haemolytic_anemia)] <- 0L

  # liver copper (Rhodanine should only trigger if liver_copper is missing or unquantifiable)
  lcopper_score <- rep(0L, length(liver_copper))
  lcopper_score[which(liver_copper > 4)] <- 2L
  lcopper_score[which(liver_copper >= 0.8 & liver_copper <= 4)] <- 1L
  lcopper_score[which(liver_copper > 0 & liver_copper < 0.8)] <- -1L
  lcopper_score[which(rhodanine_granules & (is.na(liver_copper) | liver_copper == 0))] <- 1L

  # urinary copper, add lab specific ULN.
  urinary_ratio <- urinary_copper / urinary_copper_uln

  ucopper_score <- rep(0L, length(urinary_copper))
  ucopper_score[which(urinary_ratio >= 1 & urinary_ratio <= 2)] <- 1L
  ucopper_score[which(urinary_ratio > 2)] <- 2L

  if (!is.null(penicillamine_copper)) {
    penicillamine_ratio <- penicillamine_copper / urinary_copper_uln

    ucopper_score[which(urinary_ratio < 1 & penicillamine_ratio >= 5)] <- 2L
  }

  # Mutations
  mutation_score <- rep(0L, length(mutated_chromosomes))
  mutation_score[which(mutated_chromosomes == 0)] <- 0L
  mutation_score[which(mutated_chromosomes == 1)] <- 1L
  mutation_score[which(mutated_chromosomes == 2)] <- 4L


  score <- kf_score + neuro_score + cerulo_score +
    anemia_score + lcopper_score +
    ucopper_score + mutation_score

  score
}
