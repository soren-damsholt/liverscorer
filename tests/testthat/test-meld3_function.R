# INPUT VALIDATION TESTS

test_that("invalid sex produces an error", {
  expect_error(
    meld3(
      sex = 1, age = 22,
      creatinine = 120, bilirubin = 80,
      inr = 2.0, sodium = 130, albumin = 30,
      unit = "SI"
    ),
    "sex must be either 'male' or 'female'"
  )
})


test_that("non-numeric laboratory values produce an error", {
  expect_error(
    meld3(
      sex = "female", age = 22,
      creatinine = "120", bilirubin = 80,
      inr = 2.0, sodium = 130, albumin = 30,
      unit = "SI"
    ),
    "numeric"
  )
})


test_that("invalid unit produces an error", {
  expect_error(
    meld3(
      sex = "female", age = 22,
      creatinine = 120, bilirubin = 80,
      inr = 2.0, sodium = 130, albumin = 30,
      unit = "sI"
    ),
    "unit must be one of: US, SI"
  )
})


test_that("unit must be scalar", {
  expect_error(
    meld3(
      sex = "female", age = 22,
      creatinine = 120, bilirubin = 80,
      inr = 2.0, sodium = 130, albumin = 30,
      unit = c("US", "SI")
    ),
    "unit must be one of"
  )
})


test_that("invalid age produces an error", {
  expect_error(
    meld3(
      sex = "female", age = 11,
      creatinine = 120, bilirubin = 80,
      inr = 2.0, sodium = 130, albumin = 30,
      unit = "SI"
    ),
    "12 years and older"
  )
})


test_that("invalid dialysis setting produces an error", {
  expect_error(
    meld3(
      sex = "female", age = 22,
      creatinine = 120, bilirubin = 80,
      inr = 2.0, sodium = 130, albumin = 30,
      dialysis = "yes",
      unit = "SI"
    ),
    "dialysis must be TRUE or FALSE"
  )
})


test_that("missing dialysis setting produces an error", {
  expect_error(
    meld3(
      sex = "female", age = 22,
      creatinine = 120, bilirubin = 80,
      inr = 2.0, sodium = 130, albumin = 30,
      dialysis = NA,
      unit = "SI"
    ),
    "dialysis must be TRUE or FALSE"
  )
})

# VECTOR LENGTH VALIDATION TESTS

test_that("differing patient-level vector lengths produce an error", {
  expect_error(
    meld3(
      sex = "female",
      age = c(22, 72),
      creatinine = 120,
      bilirubin = 80,
      inr = 2.0,
      sodium = 130,
      albumin = 30,
      unit = "SI"
    ),
    "All input vectors must have the same length"
  )
})


test_that("empty patient-level inputs produce an error", {
  expect_error(
    meld3(
      sex = character(0),
      age = numeric(0),
      creatinine = numeric(0),
      bilirubin = numeric(0),
      inr = numeric(0),
      sodium = numeric(0),
      albumin = numeric(0),
      unit = "SI"
    ),
    "Input vectors must contain at least one observation"
  )
})


test_that("dialysis may be scalar or match patient-level input length", {
  expect_no_error(
    meld3(
      sex = c("male", "female"),
      age = c(22, 32),
      creatinine = c(1.2, 1.5),
      bilirubin = c(2, 3),
      inr = c(1.5, 2),
      sodium = c(130, 135),
      albumin = c(3, 3.2),
      dialysis = FALSE,
      unit = "US"
    )
  )
  
  expect_no_error(
    meld3(
      sex = c("male", "female"),
      age = c(22, 32),
      creatinine = c(1.2, 1.5),
      bilirubin = c(2, 3),
      inr = c(1.5, 2),
      sodium = c(130, 135),
      albumin = c(3, 3.2),
      dialysis = c(FALSE, TRUE),
      unit = "US"
    )
  )
})


test_that("dialysis with invalid vector length produces an error", {
  expect_error(
    meld3(
      sex = c("male", "female"),
      age = c(22, 32),
      creatinine = c(1.2, 1.5),
      bilirubin = c(2, 3),
      inr = c(1.5, 2),
      sodium = c(130, 135),
      albumin = c(3, 3.2),
      dialysis = c(FALSE, TRUE, FALSE),
      unit = "US"
    ),
    "dialysis must have length 1"
  )
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
test_that("SI and US units give equivalent results", {
  us <- meld3(
    sex = "male",
    age = 22,
    creatinine = 1.2,
    bilirubin = 2,
    inr = 1.5,
    sodium = 130,
    albumin = 3,
    unit = "US"
  )
  
  si <- meld3(
    sex = "male",
    age = 22,
    creatinine = 1.2 / 0.0113,
    bilirubin = 2 / 0.0584,
    inr = 1.5,
    sodium = 130,
    albumin = 3 / 0.1,
    unit = "SI"
  )
  
  expect_equal(us, si)
})

# INPUT BOUNDING TESTS

test_that("creatinine is bounded between 1 and 3 mg/dL", {
  low <- meld3(
    "male", 22,
    creatinine = 0.5,
    bilirubin = 2,
    inr = 1.5,
    sodium = 130,
    albumin = 3
  )
  
  at_low <- meld3(
    "male", 22,
    creatinine = 1,
    bilirubin = 2,
    inr = 1.5,
    sodium = 130,
    albumin = 3
  )
  
  high <- meld3(
    "male", 22,
    creatinine = 10,
    bilirubin = 2,
    inr = 1.5,
    sodium = 130,
    albumin = 3
  )
  
  at_high <- meld3(
    "male", 22,
    creatinine = 3,
    bilirubin = 2,
    inr = 1.5,
    sodium = 130,
    albumin = 3
  )
  
  expect_equal(low, at_low)
  expect_equal(high, at_high)
})


test_that("bilirubin and INR have lower bounds of 1", {
  bili_low <- meld3(
    "male", 22, 1.2, 0.2, 1.5, 130, 3
  )
  
  bili_one <- meld3(
    "male", 22, 1.2, 1, 1.5, 130, 3
  )
  
  inr_low <- meld3(
    "male", 22, 1.2, 2, 0.4, 130, 3
  )
  
  inr_one <- meld3(
    "male", 22, 1.2, 2, 1, 130, 3
  )
  
  expect_equal(bili_low, bili_one)
  expect_equal(inr_low, inr_one)
})


test_that("sodium is bounded between 125 and 137", {
  below <- meld3(
    "male", 22, 1.2, 2, 1.5, 100, 3
  )
  
  lower_bound <- meld3(
    "male", 22, 1.2, 2, 1.5, 125, 3
  )
  
  above <- meld3(
    "male", 22, 1.2, 2, 1.5, 150, 3
  )
  
  upper_bound <- meld3(
    "male", 22, 1.2, 2, 1.5, 137, 3
  )
  
  expect_equal(below, lower_bound)
  expect_equal(above, upper_bound)
})


test_that("albumin is bounded between 1.5 and 3.5", {
  below <- meld3(
    "male", 22, 1.2, 2, 1.5, 130, 0.5
  )
  
  lower_bound <- meld3(
    "male", 22, 1.2, 2, 1.5, 130, 1.5
  )
  
  above <- meld3(
    "male", 22, 1.2, 2, 1.5, 130, 5
  )
  
  upper_bound <- meld3(
    "male", 22, 1.2, 2, 1.5, 130, 3.5
  )
  
  expect_equal(below, lower_bound)
  expect_equal(above, upper_bound)
})

# DIALYSIS OVERRIDES CREATININE

test_that("dialysis sets creatinine to 3 mg/dL", {
  dialysis_score <- meld3(
    "male", 22,
    creatinine = 1.2,
    bilirubin = 2,
    inr = 1.5,
    sodium = 130,
    albumin = 3,
    dialysis = TRUE
  )
  
  creatinine_three_score <- meld3(
    "male", 22,
    creatinine = 3,
    bilirubin = 2,
    inr = 1.5,
    sodium = 130,
    albumin = 3,
    dialysis = FALSE
  )
  
  expect_equal(dialysis_score, creatinine_three_score)
})


test_that("vectorized dialysis affects only selected observations", {
  result <- meld3(
    sex = c("male", "male"),
    age = c(22, 22),
    creatinine = c(1.2, 1.2),
    bilirubin = c(2, 2),
    inr = c(1.5, 1.5),
    sodium = c(130, 130),
    albumin = c(3, 3),
    dialysis = c(FALSE, TRUE)
  )
  
  expect_false(result[1] == result[2])
})

# CALCULATION TESTS

test_that("adult male MELD 3.0 is calculated correctly", {
  expect_equal(
    meld3(
      sex = "male",
      age = 22,
      creatinine = 1.2,
      bilirubin = 2,
      inr = 1.5,
      sodium = 130,
      albumin = 3
    ),
    20
  )
})


test_that("adult female MELD 3.0 includes female coefficient", {
  expect_equal(
    meld3(
      sex = "female",
      age = 22,
      creatinine = 1.2,
      bilirubin = 2,
      inr = 1.5,
      sodium = 130,
      albumin = 3
    ),
    22
  )
})


test_that("adolescent MELD 3.0 does not depend on sex", {
  male <- meld3(
    "male", 16,
    1.2, 2, 1.5, 130, 3
  )
  
  female <- meld3(
    "female", 16,
    1.2, 2, 1.5, 130, 3
  )
  
  expect_equal(male, 22)
  expect_equal(female, 22)
  expect_equal(male, female)
})


test_that("age exactly 18 uses adult formula", {
  expect_equal(
    meld3(
      "male", 18,
      1.2, 2, 1.5, 130, 3
    ),
    20
  )
})


test_that("age exactly 12 uses adolescent formula", {
  expect_equal(
    meld3(
      "male", 12,
      1.2, 2, 1.5, 130, 3
    ),
    22
  )
})

# VECTORIZATION and SCALAR RECYCLING
# VECTORIZATION

test_that("meld3 returns one score per patient", {
  result <- meld3(
    sex = c("male", "female", "male"),
    age = c(22, 32, 16),
    creatinine = c(1.2, 1.5, 2),
    bilirubin = c(2, 3, 4),
    inr = c(1.5, 2, 2.5),
    sodium = c(130, 135, 128),
    albumin = c(3, 3.2, 2.5)
  )
  
  expect_type(result, "double")
  expect_length(result, 3)
})


test_that("scalar dialysis is applied across a vector", {
  result_scalar <- meld3(
    sex = c("male", "female"),
    age = c(22, 32),
    creatinine = c(1.2, 1.5),
    bilirubin = c(2, 3),
    inr = c(1.5, 2),
    sodium = c(130, 135),
    albumin = c(3, 3.2),
    dialysis = TRUE
  )
  
  result_vector <- meld3(
    sex = c("male", "female"),
    age = c(22, 32),
    creatinine = c(1.2, 1.5),
    bilirubin = c(2, 3),
    inr = c(1.5, 2),
    sodium = c(130, 135),
    albumin = c(3, 3.2),
    dialysis = c(TRUE, TRUE)
  )
  
  expect_equal(result_scalar, result_vector)
})