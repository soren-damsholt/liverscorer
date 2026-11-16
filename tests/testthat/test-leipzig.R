test_that("leipzig scores correctly", {
  calculated <- leipzig(kayser_fleischer = TRUE, 
                        neurological_symptoms = "absent",
                        ceruloplasmin = 0.2,
                        haemolytic_anemia = FALSE,
                        liver_copper = 0.8,
                        urinary_copper = 1,
                        urinary_copper_uln = 1,
                        mutated_chromosomes = 0)
  
  expected <- 5

  expect_equal(calculated, expected)
  
})

test_that("Ceruloplasmin thresholds score correctly", {
  baseline <- list(
    kayser_fleischer = FALSE,
    neurological_symptoms = "absent",
    ceruloplasmin = 0.25,
    haemolytic_anemia = FALSE,
    liver_copper = 0,
    rhodanine_granules = FALSE,
    urinary_copper = 0,
    urinary_copper_uln = 1,
    penicillamine_copper = NULL,
    mutated_chromosomes = 0
  )
  
  values <- c(0.09, 0.10, 0.20, 0.21)
  
  results <- sapply(values, function(value) {
    inputs <- baseline
    inputs[["ceruloplasmin"]] <- value
    do.call(leipzig, inputs)
  })
  
  expected <- c(2, 1, 1, 0)
  expect_equal(results, expected)

})

test_that("Liver copper thresholds score correctly", {
  baseline <- list(
    kayser_fleischer = FALSE,
    neurological_symptoms = "absent",
    ceruloplasmin = 0.25,
    haemolytic_anemia = FALSE,
    liver_copper = 0,
    rhodanine_granules = FALSE,
    urinary_copper = 0,
    urinary_copper_uln = 1,
    penicillamine_copper = NULL,
    mutated_chromosomes = 0
  )
  
  values <- c(0.79, 0.80, 4.00, 4.01)
  
  results <- sapply(values, function(value) {
    inputs <- baseline
    inputs[["liver_copper"]] <- value
    do.call(leipzig, inputs)
  })
  
  expected <- c(-1, 1, 1, 2)
  expect_equal(results, expected)
  
})

test_that("Urinary copper thresholds score correctly", {
  baseline <- list(
    kayser_fleischer = FALSE,
    neurological_symptoms = "absent",
    ceruloplasmin = 0.25,
    haemolytic_anemia = FALSE,
    liver_copper = 0,
    rhodanine_granules = FALSE,
    urinary_copper = 0,
    urinary_copper_uln = 1,
    penicillamine_copper = NULL,
    mutated_chromosomes = 0
  )
  
  values <- c(0.99, 1.00, 2, 2.01)
  
  results <- sapply(values, function(value) {
    inputs <- baseline
    inputs[["urinary_copper"]] <- value
    do.call(leipzig, inputs)
  })
  
  expected <- c(0, 1, 1, 2)
  expect_equal(results, expected)
  
})

test_that("penicillamine challenge overrrides low urinary copper", {
  baseline <- list(
    kayser_fleischer = FALSE,
    neurological_symptoms = "absent",
    ceruloplasmin = 0.25,
    haemolytic_anemia = FALSE,
    liver_copper = 0,
    rhodanine_granules = FALSE,
    urinary_copper = 0,
    urinary_copper_uln = 1,
    penicillamine_copper = NULL,
    mutated_chromosomes = 0
  )
  
  without_challenge <- do.call(leipzig, baseline)
  baseline[["penicillamine_copper"]] <- 5
  with_challenge <- do.call(leipzig, baseline)
  
  expect_equal(with_challenge - without_challenge, 2)
  
})

test_that("Numeric inputs must be numeric", {
  baseline <- list(
    kayser_fleischer = FALSE,
    neurological_symptoms = "absent",
    ceruloplasmin = 0.25,
    haemolytic_anemia = FALSE,
    liver_copper = 1,
    rhodanine_granules = FALSE,
    urinary_copper = 1,
    urinary_copper_uln = 1,
    penicillamine_copper = NULL,
    mutated_chromosomes = 0
  )
  
  numeric_names <- c("ceruloplasmin", 
                     "liver_copper", 
                     "urinary_copper",
                     "urinary_copper_uln",
                     "mutated_chromosomes")
  
  for(name in numeric_names) {
    inputs <- baseline
    inputs[[name]] <- "wrong"
    
    expect_error(do.call(leipzig, inputs),
                 "numeric", info = name)
  }
  
  inputs <- baseline
  inputs$penicillamine_copper <- "wrong"
  
  expect_error(do.call(leipzig, inputs),
               "penicillamine_copper must be numeric or NULL")
  
})

test_that("NAs will throw warning", {
  
  expect_warning(
    calculated <- leipzig(kayser_fleischer = TRUE, 
                          neurological_symptoms = "absent",
                          ceruloplasmin = NA,
                          haemolytic_anemia = FALSE,
                          liver_copper = 0.8,
                          urinary_copper = 1,
                          urinary_copper_uln = 1,
                          mutated_chromosomes = 0),
    "Missing inputs may affect component scoring"
  )
  
})

test_that("Rhodanine granules only trigger if liver copper is unquantifiable", {
  calculated_quantifiable <- leipzig(kayser_fleischer = TRUE, 
                                     neurological_symptoms = "absent",
                                     ceruloplasmin = 0.8,
                                     haemolytic_anemia = FALSE,
                                     liver_copper = 0.75,
                                     urinary_copper = 1,
                                     urinary_copper_uln = 1,
                                     mutated_chromosomes = 0,
                                     rhodanine_granules = TRUE
                                     )
  expect_equal(calculated_quantifiable, 2)
  
  calculated_unquantifiable <- leipzig(kayser_fleischer = TRUE, 
                                       neurological_symptoms = "absent",
                                       ceruloplasmin = 0.8,
                                       haemolytic_anemia = FALSE,
                                       liver_copper = 0,
                                       urinary_copper = 1,
                                       urinary_copper_uln = 1,
                                       mutated_chromosomes = 0,
                                       rhodanine_granules = TRUE
  )
  
  expect_equal(calculated_unquantifiable, 4)
  
  expect_warning(
    calculated <- leipzig(kayser_fleischer = TRUE, 
                          neurological_symptoms = "absent",
                          ceruloplasmin = 0.8,
                          haemolytic_anemia = FALSE,
                          liver_copper = NA,
                          urinary_copper = 1,
                          urinary_copper_uln = 1,
                          mutated_chromosomes = 0,
                          rhodanine_granules = TRUE
    ),
    "Missing inputs may affect component scoring"
  )
  
  expect_equal(calculated, 4)
})

test_that("Vectorization works", {
  
  calculated <- leipzig(kayser_fleischer = c(TRUE, FALSE, FALSE), 
          neurological_symptoms = c("absent", "mild", "severe"),
          ceruloplasmin = c(0.08, 0.1, 0.25),
          haemolytic_anemia = c(FALSE, TRUE, FALSE),
          liver_copper = c(0.5, 1, 5),
          urinary_copper = c(0.5, 1.5, 2.5),
          urinary_copper_uln = c(1, 1, 1),
          mutated_chromosomes = c(0, 1, 2))
  
  expected <- c(3, 6, 10)
  
  expect_equal(calculated, expected)
})

test_that("Capitalization does not break neurological inputs", {
  inputs <- c("SEVERE", "Severe", "SeVerE")
  
    for(var in inputs) {
      calculated <- leipzig(kayser_fleischer = FALSE,
                            neurological_symptoms = var,
                            ceruloplasmin = 0.25,
                            haemolytic_anemia = FALSE,
                            liver_copper = 0.5, 
                            rhodanine_granules = FALSE,
                            urinary_copper = 1,
                            urinary_copper_uln = 1,
                            penicillamine_copper = NULL,
                            mutated_chromosomes = 0
                              )
      expected <- 2
      expect_equal(calculated, expected)
    }
})

test_that("Wrong neurological inputs produces error", {
  inputs <- c("Absint", "Svre", "moderate", "ayo")
  for(var in inputs) {
    expect_error(leipzig(kayser_fleischer = FALSE,
            neurological_symptoms = var,
            ceruloplasmin = 0.25,
            haemolytic_anemia = FALSE,
            liver_copper = 0.5, 
            rhodanine_granules = FALSE,
            urinary_copper = 1,
            urinary_copper_uln = 1,
            penicillamine_copper = NULL,
            mutated_chromosomes = 0
    ))
  }
  
  expect_error(leipzig(kayser_fleischer = FALSE,
                       neurological_symptoms = 1,
                       ceruloplasmin = 0.25,
                       haemolytic_anemia = FALSE,
                       liver_copper = 0.5, 
                       rhodanine_granules = FALSE,
                       urinary_copper = 1,
                       urinary_copper_uln = 1,
                       penicillamine_copper = NULL,
                       mutated_chromosomes = 0
  ),
  "character vector")
  
})

test_that("Wrong type input in required logical fields produces stop", {
  
  expect_error(leipzig(kayser_fleischer = 2, 
                       neurological_symptoms = "absent",
                       ceruloplasmin = 2,
                       haemolytic_anemia = FALSE,
                       liver_copper = 0.8,
                       urinary_copper = 1,
                       urinary_copper_uln = 1,
                       mutated_chromosomes = 0)
               )
  expect_error(leipzig(kayser_fleischer = TRUE, 
                       neurological_symptoms = "absent",
                       ceruloplasmin = 2,
                       haemolytic_anemia = 2,
                       liver_copper = 0.8,
                       urinary_copper = 1,
                       urinary_copper_uln = 1,
                       mutated_chromosomes = 0)
               )
  
  expect_error(leipzig(kayser_fleischer = FALSE, 
                       neurological_symptoms = "absent",
                       ceruloplasmin = 2,
                       haemolytic_anemia = FALSE,
                       liver_copper = 0.8,
                       urinary_copper = 1,
                       urinary_copper_uln = 1,
                       mutated_chromosomes = 0,
                       rhodanine_granules = 2)
               )
})

test_that("Mutation rejects values outside of 0-2", {
  inputs <- c(-1, -1.5, 1.5, 2.5, 4, 17)
  for(var in inputs) {
    expect_error(leipzig(kayser_fleischer = FALSE,
                         neurological_symptoms = "absent",
                         ceruloplasmin = 0.25,
                         haemolytic_anemia = FALSE,
                         liver_copper = 0.5, 
                         rhodanine_granules = FALSE,
                         urinary_copper = 1,
                         urinary_copper_uln = 1,
                         penicillamine_copper = NULL,
                         mutated_chromosomes = var
    ))
  }
})

test_that("Mutations are calculated correctly", {
  calculated <- leipzig(kayser_fleischer = c(FALSE, FALSE, FALSE), 
                        neurological_symptoms = c("absent", "absent", "absent"),
                        ceruloplasmin = c(0.25, 0.25, 0.25),
                        haemolytic_anemia = c(FALSE, FALSE, FALSE),
                        liver_copper = c(0, 0, 0),
                        rhodanine_granules = FALSE,
                        urinary_copper = c(0.5, 0.5, 0.5),
                        urinary_copper_uln = c(1, 1, 1),
                        mutated_chromosomes = c(0, 1, 2))
  
  expected <- c(0, 1, 4)
  
  expect_equal(calculated, expected)
  
})

test_that("Impossible numeric values throws error", {
  expect_error(leipzig(kayser_fleischer = FALSE,
                       neurological_symptoms = "absent",
                       ceruloplasmin = -0.1,
                       haemolytic_anemia = FALSE,
                       liver_copper = 0.5, 
                       rhodanine_granules = FALSE,
                       urinary_copper = 1,
                       urinary_copper_uln = 1,
                       penicillamine_copper = NULL,
                       mutated_chromosomes = 0))
  
  expect_error(leipzig(kayser_fleischer = FALSE,
                       neurological_symptoms = "absent",
                       ceruloplasmin = 2.5,
                       haemolytic_anemia = FALSE,
                       liver_copper = -1, 
                       rhodanine_granules = FALSE,
                       urinary_copper = 1,
                       urinary_copper_uln = 1,
                       penicillamine_copper = NULL,
                       mutated_chromosomes = 0))
  
  expect_error(leipzig(kayser_fleischer = FALSE,
                       neurological_symptoms = "absent",
                       ceruloplasmin = 0.25,
                       haemolytic_anemia = FALSE,
                       liver_copper = 0.5, 
                       rhodanine_granules = FALSE,
                       urinary_copper = -1,
                       urinary_copper_uln = 1,
                       penicillamine_copper = NULL,
                       mutated_chromosomes = 0))
  
  expect_error(leipzig(kayser_fleischer = FALSE,
                       neurological_symptoms = "absent",
                       ceruloplasmin = 0.25,
                       haemolytic_anemia = FALSE,
                       liver_copper = 0.5, 
                       rhodanine_granules = FALSE,
                       urinary_copper = 1,
                       urinary_copper_uln = 0,
                       penicillamine_copper = NULL,
                       mutated_chromosomes = 0))
  
  expect_error(leipzig(kayser_fleischer = FALSE,
                       neurological_symptoms = "absent",
                       ceruloplasmin = 0.25,
                       haemolytic_anemia = FALSE,
                       liver_copper = 0.5, 
                       rhodanine_granules = FALSE,
                       urinary_copper = 1,
                       urinary_copper_uln = -1,
                       penicillamine_copper = NULL,
                       mutated_chromosomes = 0))
  
  expect_error(leipzig(kayser_fleischer = FALSE,
                       neurological_symptoms = "absent",
                       ceruloplasmin = 0.25,
                       haemolytic_anemia = FALSE,
                       liver_copper = 0.5, 
                       rhodanine_granules = FALSE,
                       urinary_copper = 1,
                       urinary_copper_uln = 1,
                       penicillamine_copper = -1,
                       mutated_chromosomes = 0))
  })

test_that("Penicillamine copper can be NULL, scalar and vector", {
  calculated_null <- leipzig(kayser_fleischer = FALSE,
                              neurological_symptoms = "absent",
                              ceruloplasmin = 0.25,
                              haemolytic_anemia = FALSE,
                              liver_copper = 0, 
                              rhodanine_granules = FALSE,
                              urinary_copper = 0.5,
                              urinary_copper_uln = 1,
                              penicillamine_copper = NULL,
                              mutated_chromosomes = 0
                      )
  expect_equal(calculated_null, 0)
  
  calculated_scalar <- leipzig(kayser_fleischer = c(FALSE, FALSE),
                             neurological_symptoms = c("absent", "absent"),
                             ceruloplasmin = c(0.25, 0.25),
                             haemolytic_anemia = c(FALSE, FALSE),
                             liver_copper = c(0, 0), 
                             rhodanine_granules = c(FALSE, FALSE),
                             urinary_copper = c(0.5, 0.5),
                             urinary_copper_uln = c(1, 1),
                             penicillamine_copper = 5,
                             mutated_chromosomes = c(0, 0)
  )
  expect_equal(calculated_scalar, c(2, 2))
  
  calculated_vector <-leipzig(kayser_fleischer = c(FALSE, FALSE),
                              neurological_symptoms = c("absent", "absent"),
                              ceruloplasmin = c(0.25, 0.25),
                              haemolytic_anemia = c(FALSE, FALSE),
                              liver_copper = c(0, 0), 
                              rhodanine_granules = c(FALSE, FALSE),
                              urinary_copper = c(0.5, 0.5),
                              urinary_copper_uln = c(1, 1),
                              penicillamine_copper = c(4.99, 5),
                              mutated_chromosomes = c(0, 0)
                              )
  
  expect_equal(calculated_vector, c(0, 2))
  
})

test_that("Urinary copper is scored relative to the supplied ULN", {
  calculated <- leipzig(
    kayser_fleischer = c(FALSE, FALSE, FALSE),
    neurological_symptoms = c("absent", "absent", "absent"),
    ceruloplasmin = c(0.25, 0.25, 0.25),
    liver_copper = c(0, 0, 0),
    urinary_copper = c(0.5, 1, 1.5),
    urinary_copper_uln = 0.5,
    mutated_chromosomes = c(0, 0, 0)
  )
  
  expect_equal(calculated, c(1, 1, 2))
})

test_that("Scalar optional inputs apply to all observations",{
  calculated <- leipzig(kayser_fleischer = c(FALSE, FALSE),
                        neurological_symptoms = c("absent", "absent"),
                        ceruloplasmin = c(0.25, 0.25),
                        haemolytic_anemia = TRUE,
                        liver_copper = c(0.5, 0.5), 
                        rhodanine_granules = FALSE,
                        urinary_copper = c(1, 1),
                        urinary_copper_uln = 1,
                        mutated_chromosomes = c(0, 0)
  )
  expect_equal(calculated, c(1, 1))
  
})

test_that("Empty patient-level inputs are rejected", {
  expect_error(
    leipzig(kayser_fleischer = logical(0),
            neurological_symptoms = character(0),
            ceruloplasmin = numeric(0),
            liver_copper = numeric(0),
            urinary_copper = numeric(0),
            urinary_copper_uln = 1,
            mutated_chromosomes = numeric(0)),
    "at least one observation"
  )
})

test_that("Missing logical findings warn and default to zero", {
  expect_warning(
    result <- leipzig(
      kayser_fleischer = NA,
      neurological_symptoms = "absent",
      ceruloplasmin = 0.25,
      haemolytic_anemia = FALSE,
      liver_copper = 0,
      rhodanine_granules = FALSE,
      urinary_copper = 0.5,
      urinary_copper_uln = 1,
      mutated_chromosomes = 0
    ),
    "Missing inputs may affect component scoring"
  )
  
  expect_equal(result, 0)
})

test_that("vector length must be either 1 or same length", {
  expect_error(leipzig(
    kayser_fleischer = c(TRUE, FALSE),
    neurological_symptoms = "absent",
    ceruloplasmin = c(0.25, 3),
    haemolytic_anemia = c(TRUE, FALSE),
    liver_copper = c(0, 0),
    rhodanine_granules = c(TRUE, FALSE),
    urinary_copper = c(0.5, 0.5),
    urinary_copper_uln = c(1, 1),
    mutated_chromosomes = c(0, 0)
  ))
  
})

