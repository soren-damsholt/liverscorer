# INPUT VALIDATION TESTS
test_that("Input validation of invalid sex produces error", {
  expect_error(meld3(sex = 1, age = 22,
                     creatinine = "120", bilirubin = 80,
                     inr = 2.0, sodium = 130, albumin = 30, unit = "SI"),
               "sex must be either 'male' or 'female'")
})


test_that("Non-numeric laboratory values produce an error", {
  expect_error(meld3(sex = "female", age = 22, creatinine = "120", bilirubin = 80,
                     inr = 2.0, sodium = 130, albumin = 30, unit = "SI"),
               
               "One or more of your vectors are not numeric"
               )
})


test_that("Input validation of invalid unit setting produces error", {
  expect_error(meld3(sex = "female", age = 22,
                     creatinine = 120, bilirubin = 80,
                     inr = 2.0, sodium = 130, albumin = 30, unit = "sI"),
               "unit must be either 'US' or 'SI'")
})


test_that("Input validation of invalid dialysis setting produces error", {
  expect_error(meld3(sex = "female", age = 22,
                     creatinine = 120, bilirubin = 80,
                     inr = 2.0, sodium = 130, albumin = 30, unit = "SI",
                     dialysis = "yes"))
})

# VECTOR LENGTH VALIDATION TESTS

test_that("Input of differing length vectors produces an error", {
  expect_error(meld3(sex = "female", age = c(22, 72),
                     creatinine = 120, bilirubin = 80,
                     inr = 2.0, sodium = 130, albumin = 30, unit = "SI"),
               "All input vectors must have the same length")
})


test_that("Empty input produces an error", {
  expect_error(meld3(sex = character(0), age = numeric(0),
                     creatinine = numeric(0), bilirubin = numeric(0),
                     inr = numeric(0), sodium = numeric(0), albumin = numeric(0), unit = "SI"),
               "Input vectors must contain at least one observation")
})

# NA HANDLING TESTS
test_that("Input validation of NA handling", {
  expect_equal(meld3(sex = "female", age = NA, creatinine = 120, bilirubin = 80, inr = 2.0,
                     sodium = 130, albumin = 30, unit = "SI",dialysis = TRUE),
               NA_real_)
  expect_equal(meld3(sex = "female", age = 32, creatinine = NA, bilirubin = 80, inr = 2.0,
                     sodium = 130, albumin = 30, unit = "SI",dialysis = TRUE),
               NA_real_)
  expect_equal(meld3(sex = "female", age = 32, creatinine = 120, bilirubin = NA, inr = 2.0,
                     sodium = 130, albumin = 30, unit = "SI",dialysis = TRUE),
               NA_real_)
  expect_equal(meld3(sex = "female", age = 32, creatinine = 120, bilirubin = 80, inr = NA,
                     sodium = 130, albumin = 30, unit = "SI",dialysis = TRUE),
               NA_real_)
  expect_equal(meld3(sex = "female", age = 32, creatinine = 120, bilirubin = 80, inr = 2.0,
                     sodium = NA, albumin = 30, unit = "SI",dialysis = TRUE),
               NA_real_)
  expect_equal(meld3(sex = "female", age = 32, creatinine = 120, bilirubin = 80, inr = 2.0,
                     sodium = 130, albumin = NA, unit = "SI",dialysis = TRUE),
               NA_real_)
})

test_that("NA in a vector only produces a NA for the corresponding missing scores", {
  result <- meld3(sex = c("male", "female", "male", "female"),
                  age = c(22, 18, NA, 16),
                  creatinine = c(120, 130, 140, 150),
                  bilirubin = c(50, 60, 70, 90),
                  inr = c(1.5, 2.0, 2.5, 3.0),
                  sodium = c(NA, 136, 140, 150),
                  albumin = c(30, 40, 25, 20),
                  unit = "SI")
  
  expect_true(is.na(result[1]))
  expect_false(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_false(is.na(result[4]))
  
})

# UNIT CONVERSION

# UNIT BOUNDING

# DIALYSIS OVERIDING CREATININE

# CALCULATION
  # ADULT MALE
  # ADULT FEMALE
  # AGE 12-17
  # AGE EXACTLY 18
  # AGE EXACTLY 12

# VECTORIZATION and SCALAR RECYCLING