test_that("Testing that meld score calculates correctly (US + no dialysis)", {
  calculated <- meld(creatinine = 1.2, bilirubin = 5, inr = 2.0,
                     units = "US", dialysis = FALSE)
  expected  <- round((0.957 * log(1.2) +
                       0.378 * log(5) +
                       1.120 * log(2) +
                       0.643) * 10)
  expect_equal(calculated, expected)
  
})

test_that("Testing that meld score calculates correctly (US + dialysis)", {
  calculated <- meld(creatinine = 1.2, bilirubin = 5,
                     inr = 2.0, units = "US", dialysis = TRUE)
  expected  <- round((0.957 * log(4) +
                       0.378 * log(5) +
                       1.120 * log(2) +
                       0.643) * 10)
  expect_equal(calculated, expected)
})

test_that("Testing that meld score calculates correctly (SI + no dialysis)", {
  calculated <- meld(creatinine = 120, bilirubin = 80, inr = 2.0, units = "SI",
                     dialysis = FALSE)
  expected  <- round((0.957 * log(120*0.0113) +
                       0.378 * log(80*0.0584) +
                       1.120 * log(2) +
                       0.643) * 10)
  expect_equal(calculated, expected)
  
})

test_that("Testing that meld score calculates correctly (SI + dialysis)", {
  calculated <- meld(creatinine = 120, bilirubin = 80, inr = 2.0,
                     units = "SI", dialysis = TRUE)
  expected  <- round((0.957 * log(4) +
                       0.378 * log(80*0.0584) +
                       1.120 * log(2) +
                       0.643) * 10)
  expect_equal(calculated, expected)
  
})

test_that("MELD values below 1 are set to 1", {
  calculated <- meld(creatinine = 0.5, bilirubin = 0.5, inr = 0.5,
                     units = "US", dialysis = FALSE)
  
  expected <- round((0.957 * log(1) +
                       0.378 * log(1) +
                       1.120 * log(1) +
                       0.643) * 10)
  expect_equal(calculated, expected)
  
})

test_that("MELD is vectorized and handles dialysis as scalar", {
  calculated <- meld(creatinine = c(0.5, 1, 2),
                     bilirubin = c(0.5, 1, 2),
                     inr = c(0.5, 1, 2), dialysis = FALSE,
                     units = "US")
  expect_length(calculated, 3)
  expect_true(all(is.finite(calculated)))
})

test_that("MELD handles dialysis as a vector", {
  calculated <- meld(creatinine = c(0.5, 1, 2),
                     bilirubin = c(0.5, 1, 2),
                     inr = c(0.5, 1, 2), dialysis = c(FALSE, TRUE, FALSE),
                     units = "US")
  expect_length(calculated, 3)
  expect_true(all(is.finite(calculated)))
})

test_that("Invalid units produces error and stops MELD function", {
  for(value in list("Si", "SU", "Usa", "Us")) {
    expect_error(
      meld(1.2, 5, 2, units = value),
      "units must be one of: US, SI")
  }
})

test_that("Invalid dialysis setting produces an error", {
  for(value in list("yes", "no", "maybe", 1, 0, NA)){
    expect_error(
      meld(1.2, 5, 2, dialysis = value),
      "dialysis must be TRUE or FALSE"
    )}
})

test_that("Missing values produce NAs in vectors",{
  result <- meld(creatinine = c(1, NA, 2),
                 bilirubin = c(1, 2, 3),
                 inr = c(1, 1.5, 2))
  
  expect_equal(length(result), 3)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[3]))
})


test_that("Creatinine is bounded between 1 and 4", {
  result_low <- suppressWarnings(meld(0.5, 3, 2))
  result_high <- suppressWarnings(meld(5, 3, 2))
  
  expected_low <- suppressWarnings(meld(1, 3, 2))
  expected_high <- suppressWarnings(meld(4, 3, 2))
  
  expect_equal(result_low, expected_low)
  expect_equal(result_high, expected_high)
})
