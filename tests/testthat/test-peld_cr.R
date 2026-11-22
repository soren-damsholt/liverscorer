# Core structure correctness

## test that one simple scalar case against reference
test_that("PELD calculates correctly for single patient", {
  calculated <- peld_cr(age_years = 1, age_at_measurement = 12,
                        sex = "female", height = 120, weight = 60,
                        albumin = 20, bilirubin = 50, inr = 2,
                        creatinine = 190, dialysis = FALSE, units = "SI")
  expected <- 44
  expect_equal(calculated, expected)
})
## one vectorized multi-patient case
test_that("PELD calculates correctly for patient vector", {
  calculated <- peld_cr(age_years = c(1, 1.5, 2),
                        age_at_measurement = c(12, 18, 24),
                        sex = c("female", "male", "female"),
                        height = c(120, 140, 160),
                        weight = c(60, 70, 80),
                        albumin = c(20, 26, 36),
                        bilirubin = c(50, 55, 60),
                        inr = c(2, 1.9, 1.7),
                        creatinine = c(190, 150, 90),
                        dialysis = c(FALSE, FALSE, FALSE),
                        units = "SI")
  expected <- c(44, 43, 37)
  expect_equal(calculated, expected)
})

## minimum value returned is 6
test_that("PELD is capped at 6 for lower values", {
  calculated <- peld_cr(age_years = 8, age_at_measurement = 96,
                        sex = "female", height = 150, weight = 60,
                        albumin = 46, bilirubin = 5, inr = 1,
                        creatinine = 20, dialysis = FALSE, units = "SI")
  expected <- 6
  expect_equal(calculated, 6)
})

# Growth component integration
## lower of height and weight score is used
## growth z-score below -5 is capped at -5
## growth z- score above -2.1 is capped at -2.1
## sex-specific CDC references affect the score appropriately
## age_at_measurement is used for reference not age_years

# Age term
## age < 1 behaves as age = 1
## exact boundary at 1
## values between 1 and 5.5 
## > 5.5 capped at 5.5
## age >= 12 is rejected
## negative ages are rejected

# Laboratory bounds
## Albumin < 1 is bounded between 1-1.9 and > 1.9
## albumin < 1, around 4, around 40 and > 40
## INR < 1, around 2, around 10, and > 10
## creatinine <0.2, within range, and >1.3

#Dialysis
## dialysis == TRUE forces creatinine to 1.3 mg/dL
## scalar = FALSE works for a vector
## scalar = TRUE works for a vector
## vectorized dialysis status works
## missing dialysis status gives NA for the relevant observation
## creatinine already > 1.3 behaves equivalently to the cap

# Unit conversions
## equivalent US and SI inputs return identical scores
## albumin conversion
## bilirubin conversion
## creatinine coverstion
## invalid units
## invalid conversion-factor type/length ? maybe not...

# Missingness
## each required input can independently cause NA
## only the affected patient becomes NA in a vector
## one package-level missing warning per call

# Type, range and length validation
## Non-numeric lab input
## non-numeric age_at_measurement
## invalid sex
## sex capitalization is converted tolower
## non-logical dialys
## negative/zero impossible anthropometrics
## impossible lab values
## unequal patient-vector lenghts
## invalid dialysis length
## zero-length inputs.
