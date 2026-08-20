test_that("MELD-Na calculates correctly", {
  result <- meldna(
    creatinine = 2, bilirubin = 3, inr = 2, sodium = 140,
    unit = "US", dialysis = "no")
  expect_equal(result, 25)
})


test_that("Sodium is bounded at 125", {
  result_125 <- meldna(2, 3, 2, 125)
  result_120 <- meldna(2, 3, 2, 120)
  
  expect_equal(result_120, result_125)
})


test_that("Sodium is bounded at 137", {
  result_137 <- meldna(2, 3, 2, 137)
  result_140 <- meldna(2, 3, 2, 140)
  
  expect_equal(result_137, result_140)
})


test_that("MELD(i) <= 11 is not adjusted for sodium", {
  result_125 <- meldna(1, 1, 1, 125)
  result_137 <- meldna(1, 1, 1, 137)
  
  expect_equal(result_125, result_137)
  expect_equal(result_125, 6)
})


test_that("Dialysis sets creatinine to 4", {
  result <- meldna(creatinine = 1, 1, 1, 137,
                   dialysis = "yes")
  expected <- suppressWarnings(meldna(creatinine = 4, 1, 1, 137,
                     dialysis = "no"))
  
  expect_equal(result, expected)
})


test_that("SI units are converted correctly", {
  result_si <- meldna(120, 80, 2, 137, unit = "SI")
  result_us <- meldna(creatinine = 120 * 0.0113, 
                      bilirubin = 80 * 0.0584,
                      inr = 2,
                      sodium = 137,
                      unit = "US")
  expect_equal(result_si, result_us)
})


test_that("MELD-Na is vectorized", {
  result <- meldna(creatinine = c(1, 1.5, 2),
                   bilirubin = c(1, 2, 3),
                   inr = c(1, 1.5, 2),
                   sodium = c(125, 135, 140)
                   )
  expect_length(result, 3)
  expect_true(all(is.finite(result)))
})

test_that("Creatinine, bilirubin and INR are bounded at 1", {
  result <- meldna(
    creatinine = 0.5,
    bilirubin = 0.5,
    inr = 0.5,
    sodium = 137)
  
  expected <- meldna(
    creatinine = 1,
    bilirubin = 1,
    inr = 1,
    sodium = 137
  )
  
  expect_equal(result, expected)
})

test_that("Invalid unit produces an error", {
  expect_error(
    meldna(1, 1, 1, 138, unit = "Us"),
    "unit must be either 'US' or 'SI'"
  )
})

test_that("Invalid dialysis setting produces an error", {
  expect_error(
    meldna(1, 1, 1, 138, dialysis = "mnjaeh"),
    "dialysis must be either 'yes' or 'no'"
  )
})

test_that("Extreme values generate warnings (US)", {
  expect_warning(
    meldna(
      creatinine = 4, bilirubin = 1, 
      inr = 1, sodium = 138,
      unit = "US"
    ),
    "one or more lab values look unusual"
  )
})

test_that("Extreme values generate warnings (SI)", {
  expect_warning(
    meldna(creatinine = 300,
           bilirubin = 90,
           inr = 1, 
           sodium = 137,
           unit = "SI"),
    "one or more lab values look unusual"
  )
})

# Values independently verified against MDCalc on 2026-08-20.
test_that("function calculates MELD-NA in accordance with verified source", {
  result_si <- meldna(creatinine = 130, bilirubin = 90, inr = 2.0, sodium = 127, unit = "SI")
  result_us <- meldna(2, 1, 2, 128)
  
  expect_equal(result_si, 29)
  expect_equal(result_us, 27)
})




