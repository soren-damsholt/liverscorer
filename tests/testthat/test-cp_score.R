## INPUT VALIDATION
test_that("Scalar units setting throws error if wrong input", {
  wrong_inputs <- c("si", "sI", "USA", "Us", "metric")
  for(var in wrong_inputs) {
    expect_error(cp_score(22, 22, 2, "slight", 2, units = var),
                 "units must be one of: US, SI")
  }
  
})
test_that("Wrong datatype of expected numeric inputs throws error", {
  wrong_inputs <- c("22", "10")
  for(var in wrong_inputs) {
    expect_error(cp_score(var, 22, 2, "slight", 2, units = "SI"),
                 "Bilirubin, albumin, inr and 'he' must be numeric")
  }
})

test_that("HE can only be NA or a value between 0 and 4", {
  wrong_inputs <-c(-1, 5, 10)
  for(var in wrong_inputs) {
    expect_error(cp_score(22, 22, 2, "slight", var, units = "SI"),
                 "'he' must have a value between 0 and 4")
  }
})

test_that("Ascites must be a charcter vector", {
  wrong_inputs <-c(-1, 5, 10)
  for(var in wrong_inputs) {
    expect_error(cp_score(22, 22, 2, var, 3, units = "SI"),
                 "ascites must be a character vector")
  }
})

test_that("Ascites must be one of 'absent', 'slight' or 'moderate'", {
  wrong_inputs <-c("MAX", "any", "severe")
  for(var in wrong_inputs) {
    expect_error(cp_score(22, 22, 2, ascites = var, 2, units = "SI"),
                 "'ascites' must be one of 'absent', 'slight' or 'moderate'")
    }
  })

test_that("Numeric inputs must all be positive", {
  wrong_inputs <- c(-2, 0)
  for (var in wrong_inputs) {
    expect_error(cp_score(var, 22, 2, ascites = "slight", 2, units = "SI"),
                 "Lab values must all be > 0 or NA")
    }
    
    for (var in wrong_inputs) {
      expect_error(cp_score(22, var, 2, ascites = "slight", 2, units = "SI"),
                   "Lab values must all be > 0 or NA")
    }
    for (var in wrong_inputs) {
      expect_error(cp_score(22, 22, var, ascites = "slight", 2, units = "SI"),
                   "Lab values must all be > 0 or NA")
    }
  
})


## UNIT CONVERSION
test_that("Unit conversion occurs", {
  calculated <- cp_score(bilirubin = 22,
                         albumin = 22,
                         inr = 2, ascites = "slight",
                         he = 2, units = "SI")
  
  expected <- cp_score(bilirubin = 22 * 0.0584,
                       albumin = 2.2, inr = 2,
                       ascites = "slight", he = 2,
                       units = "US")

  expect_equal(calculated, expected)
})
test_that("The function handles Capitalization of ascites grades", {
  calculated <- cp_score(bilirubin = 22,
                         albumin = 22,
                         inr = 2,
                         ascites = "Slight",
                         he = 2,
                         units = "SI")
  expected <- 10
  expect_equal(calculated, expected)
})

## Score calculation
test_that("Calculates correct scores", {
  calculated <- cp_score(bilirubin = 22,
                         albumin = 22,
                         inr = 2,
                         ascites = "slight",
                         he = 2,
                         units = "SI")
  expected <- 10
  expect_equal(calculated, expected)
})

## Missing values

## Vectorization
test_that("function is fully vectorized", {
  calculated <- cp_score(bilirubin = c(22, 60, 80),
                         albumin = c(35, 28, 22),
                         inr = c(1.2, 1.8, 3),
                         ascites = c("absent", "absent", "slight"),
                         he = c(0, 1, 3),
                         units = "SI")
  
  expected <- c(6, 10, 14)
  expect_equal(calculated, expected)
})