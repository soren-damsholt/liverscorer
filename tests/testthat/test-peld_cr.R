# Core structure correctness
test_that("PELD calculates correctly for single patient", {
  calculated <- peld_cr(age_years = 1, age_at_measurement = 12,
                        sex = "female", height = 120, weight = 60,
                        albumin = 20, bilirubin = 50, inr = 2,
                        creatinine = 190, dialysis = FALSE, units = "SI")
  expected <- 44
  expect_equal(calculated, expected)
})
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

test_that("PELD-CR has a minimum at 6 for lower values", {
  calculated <- peld_cr(age_years = 8, age_at_measurement = 96,
                        sex = "female", height = 150, weight = 60,
                        albumin = 46, bilirubin = 5, inr = 1,
                        creatinine = 20, dialysis = FALSE, units = "SI")
  expected <- 6
  expect_equal(calculated, 6)
})

test_that("PELD-CR is calculated correctly with external growth validation", {
  calculated <- peld_cr(age_years = 2, albumin = 3.2, bilirubin = 2.5, 
                        inr = 1.4, creatinine = 0.4, growth_z = -3, dialysis = FALSE,
                        units = "US"
                        )
  expected <- 18
  expect_equal(calculated, expected)
})

test_that("PELD-Cr is vectorized for external growth validation", {
  calculated <-  calculated <- peld_cr(
    age_years = rep(2, 3),
    albumin = rep(3.2, 3),
    bilirubin = rep(2.5, 3),
    inr = rep(1.4, 3),
    creatinine = rep(0.4, 3),
    growth_z = c(-5, -3, -2.1),
    dialysis = FALSE,
    units = "US"
  )
  
  expected <- c(22, 18, 17)
  expect_equal(calculated, expected)
})

test_that("PELD-Cr growth calculation pathways are equivalent", {
  age <- 24
  sex <- "female"
  height <- 75
  weight <- 9
  
  z_height <- .cdc_growth_z(measurement = height, age_months = age,
                            sex = sex, reference = cdc_height)
  
  z_weight <- .cdc_growth_z(measurement = weight, age_months = age,
    sex = sex, reference = cdc_weight)
  
  growth_z <- min(z_height, z_weight)
  
  calculated_cdc <- peld_cr(age_years = 2, age_at_measurement = age,sex = sex,
    height = height,weight = weight, albumin = 3.2, bilirubin = 2.5,
    inr = 1.4,creatinine = 0.4)
  
  calculated_external <- peld_cr(age_years = 2, albumin = 3.2, bilirubin = 2.5,
                                 inr = 1.4, creatinine = 0.4, growth_z = growth_z)
  
  expect_equal(calculated_cdc, calculated_external)
})

# Growth component integration
test_that("PELD-Cr uses the lower of height and weight z-scores", {
  height <- c(75, 90)
  weight <- c(12, 8)
  age <- c(24, 24)
  sex <- c("female", "female")
  
  z_height <- .cdc_growth_z(measurement = height, age_months = age,
    sex = sex, reference = cdc_height)
  
  z_weight <- .cdc_growth_z(measurement = weight, age_months = age,
                            sex = sex, reference = cdc_weight)
  
  expect_lt(z_height[1], z_weight[1])
  expect_lt(z_weight[2], z_height[2])
  
  calculated <- peld_cr(age_years = c(2, 2), age_at_measurement = age,
    sex = sex, height = height, weight = weight, albumin = c(3.2, 3.2),
    bilirubin = c(2.5, 2.5), inr = c(1.4, 1.4), creatinine = c(0.4, 0.4))
  
  expected <- peld_cr(age_years = c(2, 2), albumin = c(3.2, 3.2),
    bilirubin = c(2.5, 2.5), inr = c(1.4, 1.4), creatinine = c(0.4, 0.4),
    growth_z = c(z_height[1], z_weight[2]))
  
  expect_equal(calculated, expected)
})

test_that("PELD-Cr bounds growth z-scores below -5 at -5", {
  
  calculated <- peld_cr(age_years = rep(2, 3), albumin = rep(3.2, 3),
    bilirubin = rep(2.5, 3), inr = rep(1.4, 3),
    creatinine = rep(0.4, 3), growth_z = c(-5, -6, -10))
  
  expect_equal(calculated, rep(calculated[1], 3))
})



test_that("PELD-Cr bounds growth z-scores above -2.1 at -2.1", {
  calculated <- peld_cr(age_years = rep(2, 4), albumin = rep(3.2, 4),
    bilirubin = rep(2.5, 4), inr = rep(1.4, 4), creatinine = rep(0.4, 4),
    growth_z = c(-2.1, -1, 0, 2))
  
  expect_equal(calculated, rep(calculated[1], 4))
})

test_that("PELD-Cr increases with worsening growth restriction", {
  
  calculated <- peld_cr(age_years = rep(2, 4),albumin = rep(3.2, 4),
    bilirubin = rep(2.5, 4), inr = rep(1.4, 4),
    creatinine = rep(0.4, 4), growth_z = c(-2.1, -3, -4, -5)
  )
  
  expect_true(all(diff(calculated) > 0))
})

test_that("PELD-Cr uses sex-specific CDC growth references", {
  
  calculated <- peld_cr(age_years = c(2, 2), age_at_measurement = c(24, 24),
    sex = c("male", "female"), height = c(75, 75), weight = c(10, 10),
    albumin = c(3.2, 3.2), bilirubin = c(2.5, 2.5), inr = c(1.4, 1.4),
    creatinine = c(0.4, 0.4))
  
  z_height <- .cdc_growth_z(measurement = c(75, 75), age_months = c(24, 24),
    sex = c("male", "female"),reference = cdc_height)
  
  z_weight <- .cdc_growth_z(measurement = c(10, 10), age_months = c(24, 24),
    sex = c("male", "female"), reference = cdc_weight)
  
  expected <- peld_cr(age_years = c(2, 2), albumin = c(3.2, 3.2), bilirubin = c(2.5, 2.5),
    inr = c(1.4, 1.4), creatinine = c(0.4, 0.4), growth_z = pmin(z_height, z_weight))
  
  expect_equal(calculated, expected)
})


test_that("PELD-Cr uses age_at_measurement for CDC growth calculation", {
  calculated <- peld_cr(age_years = c(2, 2),
    age_at_measurement = c(18, 24),sex = c("female", "female"),
    height = c(75, 75), weight = c(10, 10),
    albumin = c(3.2, 3.2), bilirubin = c(2.5, 2.5),
    inr = c(1.4, 1.4), creatinine = c(0.4, 0.4))
  
  expect_gt(calculated[2], calculated[1])
})

test_that("PELD-Cr bounds ages below 1 year at 1", {
  calculated <- peld_cr(age_years = c(0, 0.5, 0.99, 1),
    albumin = rep(3.2, 4),bilirubin = rep(2.5, 4),
    inr = rep(1.4, 4),creatinine = rep(0.4, 4),
    growth_z = rep(-3, 4)
  )
  
  expect_equal(calculated, rep(calculated[4], 4))
})

test_that("PELD-Cr applies a continuous age term between 1 and 5.5 years", {
  age_years <- c(1, 2, 3, 4, 5, 5.5)
  
  calculated <- peld_cr(age_years = age_years,
    albumin = rep(3.2, 6), bilirubin = rep(2.5, 6),
    inr = rep(1.4, 6), creatinine = rep(0.4, 6),
    growth_z = rep(-3, 6))
  
  constant_terms <- -1.842 * log(1.9) +
    0.7854 * log(2.5) + 0.3434 * log(4) +
    1.981 * log(1.4) + 0.7298 * log(2) +
    -0.1807 * (-3) +
    1.453 * log(0.4)
  
  expected <- round((constant_terms - 0.1967 * age_years + 1.5287) * 10 + 2.82)
  
  expect_equal(calculated, expected)
})

test_that("PELD-Cr bounds ages above 5.5 years at 5.5", {
  calculated <- peld_cr(age_years = c(5.5, 5.51, 6, 8, 11.99),
    albumin = rep(3.2, 5), bilirubin = rep(2.5, 5),
    inr = rep(1.4, 5), creatinine = rep(0.4, 5),
    growth_z = rep(-3, 5))
  
  expect_equal(calculated, rep(calculated[1], 5))
})

test_that("PELD-Cr rejects ages of 12 years or older", {
  for (age in c(12, 12.01, 15)) {
    expect_error(peld_cr(
        age_years = age, albumin = 3.2,
        bilirubin = 2.5, inr = 1.4,
        creatinine = 0.4, growth_z = -3),
      "age_years")}
})
test_that("PELD-Cr rejects negative ages", {
  expect_error(peld_cr(age_years = -0.01,
      albumin = 3.2, bilirubin = 2.5,
      inr = 1.4, creatinine = 0.4,
      growth_z = -3),
    "age_years")
})

# Laboratory bounds
test_that("PELD-Cr applies albumin bounds correctly", {
  albumin <- c(0.5, 0.99, 1, 1.2, 1.5, 1.8, 1.9, 2, 4)
  n <- length(albumin)
  calculated <- peld_cr(age_years = rep(2, n),
    albumin = albumin, bilirubin = rep(5, n),
    inr = rep(2.5, n), creatinine = rep(0.6, n),
    growth_z = rep(-3, n))
  
  expected <- c(49, 49, 49, 46, 42, 38, 37, 37, 37)
  
  expect_equal(calculated, expected)})

test_that("PELD-Cr applies bilirubin bounds correctly", {
  bilirubin <- c(0, 0.5, 1, 2, 3.99, 4, 4.01,
                 10, 20, 39.99, 40, 40.01, 60)
  
  n <- length(bilirubin)
  
  calculated <- peld_cr(age_years = rep(2, n),
    albumin = rep(1.5, n), bilirubin = bilirubin,
    inr = rep(2.5, n), creatinine = rep(0.6, n),
    growth_z = rep(-3, n))
  
  expected <- c(30, 30, 30, 35, 41,
                41, 41, 44, 46, 49, 49, 49, 49)
  expect_equal(calculated, expected)
})
test_that("PELD-Cr applies INR bounds correctly", {
  inr <- c(0.5, 0.99, 1, 1.5, 1.99, 2,
           2.01, 4, 9.99, 10, 10.01, 15)
  n <- length(inr)
  
  calculated <- peld_cr(age_years = rep(2, n),
    albumin = rep(1.5, n), bilirubin = rep(5, n),
    inr = inr, creatinine = rep(0.6, n),
    growth_z = rep(-3, n))
  
  expected <- c(26, 26, 26, 34, 40, 40,
                40, 45, 52, 52, 52, 52)
  
  expect_equal(calculated, expected)
})
test_that("PELD-Cr applies creatinine bounds correctly", {
  creatinine <- c(0.1, 0.19, 0.2, 0.3, 0.6,
                  1,1.29, 1.3, 1.31, 2)
  
  n <- length(creatinine)
  
  calculated <- peld_cr(age_years = rep(2, n), albumin = rep(1.5, n),
    bilirubin = rep(5, n), inr = rep(2.5, n), creatinine = creatinine,
    growth_z = rep(-3, n),dialysis = FALSE)
  
  expected <- c(26, 26, 26, 31, 42,
                49, 53, 53, 53, 53)
  expect_equal(calculated, expected)
})

#Dialysis
test_that("PELD-Cr sets creatinine to 1.3 when dialysis is TRUE", {
  
  calculated <- peld_cr(age_years = c(2, 2), albumin = c(1.5, 1.5),
    bilirubin = c(5, 5), inr = c(2.5, 2.5), creatinine = c(0.4, 1.3),
    growth_z = c(-3, -3), dialysis = c(TRUE, FALSE),
    units = "US")
  
  expect_equal(calculated[1], calculated[2])
})

test_that("PELD-Cr recycles scalar FALSE dialysis status", {
  
  calculated <- peld_cr(age_years = rep(2, 3),
    albumin = rep(1.5, 3), bilirubin = rep(5, 3),
    inr = rep(2.5, 3), creatinine = c(0.4, 0.8, 1.2),
    growth_z = rep(-3, 3),dialysis = FALSE
  )
  
  expected <- peld_cr(age_years = rep(2, 3),
    albumin = rep(1.5, 3), bilirubin = rep(5, 3),
    inr = rep(2.5, 3), creatinine = c(0.4, 0.8, 1.2),
    growth_z = rep(-3, 3), dialysis = c(FALSE, FALSE, FALSE)
  )
  expect_equal(calculated, expected)
  expect_true(all(diff(calculated) > 0))
})

test_that("PELD-Cr recycles scalar TRUE dialysis status", {
  calculated <- peld_cr(age_years = rep(2, 3),
    albumin = rep(1.5, 3), bilirubin = rep(5, 3),
    inr = rep(2.5, 3), creatinine = c(0.4, 0.8, 1.2),
    growth_z = rep(-3, 3), dialysis = TRUE)
  
  expected <- peld_cr(age_years = rep(2, 3),
    albumin = rep(1.5, 3), bilirubin = rep(5, 3),
    inr = rep(2.5, 3), creatinine = rep(1.3, 3),
    growth_z = rep(-3, 3), dialysis = FALSE)
  
  expect_equal(calculated, expected)
})

test_that("PELD-Cr applies dialysis status independently by observation", {
  calculated <- peld_cr(age_years = rep(2, 4), albumin = rep(1.5, 4),
    bilirubin = rep(5, 4), inr = rep(2.5, 4), creatinine = rep(0.4, 4),
    growth_z = rep(-3, 4), dialysis = c(FALSE, TRUE, FALSE, TRUE))
  
  expected <- peld_cr(age_years = rep(2, 4), albumin = rep(1.5, 4),
    bilirubin = rep(5, 4), inr = rep(2.5, 4), creatinine = c(0.4, 1.3, 0.4, 1.3),
    growth_z = rep(-3, 4),dialysis = FALSE)
  
  expect_equal(calculated, expected)
  expect_gt(calculated[2], calculated[1])
  expect_equal(calculated[1], calculated[3])
  expect_equal(calculated[2], calculated[4])
})

test_that("PELD-Cr propagates missing dialysis status by observation", {
  expect_warning(
    calculated <- peld_cr(age_years = rep(2, 3),
      albumin = rep(1.5, 3), bilirubin = rep(5, 3),
      inr = rep(2.5, 3), creatinine = rep(0.4, 3),
      growth_z = rep(-3, 3), dialysis = c(FALSE, NA, TRUE)
    ),
    "Missing inputs")
  
  expect_length(calculated, 3)
  expect_false(is.na(calculated[1]))
  expect_true(is.na(calculated[2]))
  expect_false(is.na(calculated[3]))
  
  expect_gt(calculated[3], calculated[1])
})

test_that("PELD-Cr dialysis adjustment does not exceed the creatinine bound", {
  calculated <- peld_cr(age_years = rep(2, 6),
    albumin = rep(1.5, 6), bilirubin = rep(5, 6),
    inr = rep(2.5, 6), creatinine = c(1.3, 1.5, 3, 1.3, 1.5, 3),
    growth_z = rep(-3, 6),dialysis = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  
  expect_equal(calculated, rep(calculated[1], 6))
})

test_that("PELD-Cr applies dialysis adjustment after SI conversion", {
  calculated <- peld_cr(age_years = 2, albumin = 15, bilirubin = 85,
    inr = 2.5, creatinine = 35, growth_z = -3, dialysis = TRUE,
    units = "SI")
  
  expected <- peld_cr(age_years = 2, albumin = 15, bilirubin = 85,
    inr = 2.5, creatinine = 1.3 / 0.0113, growth_z = -3, dialysis = FALSE,
    units = "SI")
  
  expect_equal(calculated, expected)
})

# Unit conversions
test_that("PELD-Cr gives equivalent scores for US and SI units", {
  calculated_us <- peld_cr(age_years = c(2, 4, 6),
    albumin = c(1.2, 1.5, 1.8), bilirubin = c(2, 5, 15),
    inr = c(1.5, 2, 3), creatinine = c(0.4, 0.8, 1.2),
    growth_z = c(-2.5, -3, -4), units = "US")
  
  calculated_si <- peld_cr(age_years = c(2, 4, 6),
    albumin = c(1.2, 1.5, 1.8) / 0.1, bilirubin = c(2, 5, 15) / 0.0584,
    inr = c(1.5, 2, 3), creatinine = c(0.4, 0.8, 1.2) / 0.0113,
    growth_z = c(-2.5, -3, -4), units = "SI")
  
  expect_equal(calculated_si, calculated_us)
})

test_that("PELD-Cr applies the albumin conversion factor correctly", {
  
  calculated <- peld_cr(age_years = 2, albumin = 15,
    bilirubin = 85, inr = 2.5, creatinine = 50,
    growth_z = -3, units = "SI", albumin_conversion_factor = 0.08)
  
  expected <- peld_cr(age_years = 2, albumin = 1.2,
    bilirubin = 85 * 0.0584, inr = 2.5,
    creatinine = 50 * 0.0113, growth_z = -3,
    units = "US")
  
  expect_equal(calculated, expected)
})

test_that("PELD-Cr applies the bilirubin conversion factor correctly", {
  
  calculated <- peld_cr(age_years = 2, albumin = 15, bilirubin = 85,
    inr = 2.5, creatinine = 50, growth_z = -3, units = "SI",
    bilirubin_conversion_factor = 0.03)
  
  expected <- peld_cr(age_years = 2, albumin = 1.5, bilirubin = 2.55,
    inr = 2.5, creatinine = 50 * 0.0113, growth_z = -3, units = "US")
  
  expect_equal(calculated, expected)
})

test_that("PELD-Cr applies the creatinine conversion factor correctly", {
  
  calculated <- peld_cr(age_years = 2, albumin = 15, bilirubin = 85, inr = 2.5,
    creatinine = 50, growth_z = -3, dialysis = FALSE, units = "SI",
    creatinine_conversion_factor = 0.02)
  
  expected <- peld_cr(age_years = 2, albumin = 1.5, bilirubin = 85 * 0.0584,
    inr = 2.5, creatinine = 1, growth_z = -3, dialysis = FALSE, units = "US")
  
  expect_equal(calculated, expected)
})

# Missingness
test_that("PELD-Cr handles missing required inputs with external growth z-scores", {
  base_inputs <- list(age_years = 2, albumin = 1.5, bilirubin = 5,
    inr = 2.5, creatinine = 0.6, growth_z = -3, dialysis = FALSE)
  
  for (input in names(base_inputs)) {
    inputs <- base_inputs
    inputs[[input]] <- NA
    
    expect_warning(
      calculated <- do.call(peld_cr, inputs),
      "Missing inputs")
    
    expect_equal(calculated, NA_real_)}
})

test_that("PELD-Cr handles missing CDC growth inputs", {
  base_inputs <- list(age_years = 2, albumin = 1.5,
    bilirubin = 5, inr = 2.5, creatinine = 0.6,
    age_at_measurement = 24, sex = "female",
    height = 75, weight = 10, dialysis = FALSE)
  
  growth_inputs <- c("age_at_measurement", "sex",
    "height", "weight")
  
  for (input in growth_inputs) {
    
    inputs <- base_inputs
    inputs[[input]] <- base_inputs[[input]][NA_integer_]
    
    expect_warning(
      calculated <- do.call(peld_cr, inputs),
      "Missing inputs"
    )
    
    expect_equal(calculated, NA_real_)
  }
})


test_that("PELD-Cr propagates missingness independently by observation", {
  expect_warning(
    calculated <- peld_cr(age_years = c(2, 2, 2, 2, 2),
      albumin = c(1.5, NA, 1.5, 1.5, 1.5), bilirubin = c(5, 5, NA, 5, 5),
      inr = rep(2.5, 5), creatinine = rep(0.6, 5), growth_z = c(-3, -3, -3, NA, -3),
      dialysis = FALSE),
    "Missing inputs")
  
  expected <- peld_cr(age_years = 2, albumin = 1.5, bilirubin = 5,
    inr = 2.5, creatinine = 0.6, growth_z = -3)
  
  expect_equal(calculated,
    c(expected, NA_real_, NA_real_, NA_real_, expected))
})

test_that("PELD-Cr propagates missing CDC growth data by observation", {
  expect_warning(
    calculated <- peld_cr(age_years = rep(2, 5),
      albumin = rep(1.5, 5), bilirubin = rep(5, 5),
      inr = rep(2.5, 5), creatinine = rep(0.6, 5),
      age_at_measurement = rep(24, 5), sex = c("female", "female", NA, "female", "female"),
      height = c(75, NA, 75, 75, 75), weight = c(10, 10, 10, NA, 10),
      dialysis = FALSE),
    "Missing inputs")
  
  expected <- peld_cr(age_years = 2,
    albumin = 1.5, bilirubin = 5,
    inr = 2.5, creatinine = 0.6,
    age_at_measurement = 24, sex = "female",
    height = 75,weight = 10)
  
  expect_equal(calculated,
    c(expected, NA_real_, NA_real_, NA_real_, expected)
  )
})

# Type, range and length validation
test_that("PELD-Cr rejects nonnumeric patient-level inputs", {
  base_inputs <- list(age_years = 2, albumin = 1.5, bilirubin = 5,
    inr = 2.5, creatinine = 0.6, age_at_measurement = 24,
    sex = "female", height = 75, weight = 10)
  
  numeric_inputs <- c("age_years", "albumin", "bilirubin", "inr",
    "creatinine", "age_at_measurement", "height", "weight")
  
  for (input in numeric_inputs) {
    inputs <- base_inputs
    inputs[[input]] <- "invalid"
    
    expect_error(do.call(peld_cr, inputs),
      "numeric")}
})

test_that("PELD-Cr rejects invalid sex inputs", {
  invalid_sex <- list("other", "unknown", "m",
    "f", "", 1, 2, TRUE, FALSE)
  
  for (sex in invalid_sex) {
    expect_error(
      peld_cr(age_years = 2, albumin = 1.5,
        bilirubin = 5, inr = 2.5, creatinine = 0.6,
        age_at_measurement = 24, sex = sex,
        height = 75,weight = 10),
      "sex must be")}
})

test_that("PELD-Cr accepts case-insensitive sex inputs", {
  calculated <- peld_cr(age_years = rep(2, 6),
    albumin = rep(1.5, 6), bilirubin = rep(5, 6),
    inr = rep(2.5, 6), creatinine = rep(0.6, 6),
    age_at_measurement = rep(24, 6),
    sex = c("female", "Female", "FEMALE",
            "male", "Male", "MALE"),
    height = rep(75, 6), weight = rep(10, 6))
  
  expected <- c(rep(calculated[1], 3),
                rep(calculated[4], 3))
  
  expect_true(all(is.finite(calculated)))
  expect_equal(calculated, expected)
})

test_that("PELD-Cr rejects nonlogical dialysis inputs", {
  
  invalid_dialysis <- list(0, 1, "TRUE", "FALSE",
                           "yes", "no", NA_real_)
  
  for (dialysis in invalid_dialysis) {
    expect_error(
      peld_cr(age_years = 2, albumin = 1.5,
        bilirubin = 5, inr = 2.5, creatinine = 0.6,
        growth_z = -3, dialysis = dialysis),
      "dialysis")}
})

test_that("PELD-Cr rejects nonpositive anthropometric measurements", {
  
  base_inputs <- list(age_years = 2, albumin = 1.5, bilirubin = 5,
    inr = 2.5, creatinine = 0.6, age_at_measurement = 24,
    sex = "female", height = 75, weight = 10)
  
  for (input in c("height", "weight")) {
    for (value in c(-1, 0)) {
      
      inputs <- base_inputs
      inputs[[input]] <- value
      
      expect_error(
        do.call(peld_cr, inputs),
        paste(input, "must be greater than 0"))
    }
  }
})

test_that("PELD-Cr rejects impossible laboratory values", {
  
  base_inputs <- list(age_years = 2, albumin = 1.5,
    bilirubin = 5, inr = 2.5, creatinine = 0.6, growth_z = -3)
  
  invalid_values <- list(albumin = c(-1, 0),
    bilirubin = -1, inr = c(-1, 0), creatinine = c(-1, 0))
  
  for (input in names(invalid_values)) {
    for (value in invalid_values[[input]]) {
      
      inputs <- base_inputs
      inputs[[input]] <- value
      
      expect_error(
        do.call(peld_cr, inputs),
        paste(input, "must be"))}}
})

test_that("PELD-Cr rejects unequal required patient-level input lengths", {
  
  base_inputs <- list(age_years = rep(2, 3), albumin = rep(1.5, 3),
    bilirubin = rep(5, 3), inr = rep(2.5, 3), creatinine = rep(0.6, 3),
    growth_z = rep(-3, 3))
  
  required_inputs <- c("age_years", "albumin", "bilirubin",
    "inr","creatinine")
  
  for (input in required_inputs) {
    
    inputs <- base_inputs
    inputs[[input]] <- inputs[[input]][1]
    
    expect_error(do.call(peld_cr, inputs),
      "same length")}
})

test_that("PELD-Cr rejects unequal CDC growth input lengths", {
  
  base_inputs <- list(age_years = rep(2, 3), albumin = rep(1.5, 3),
    bilirubin = rep(5, 3),inr = rep(2.5, 3), creatinine = rep(0.6, 3),
    age_at_measurement = rep(24, 3), sex = rep("female", 3),
    height = rep(75, 3), weight = rep(10, 3)
  )
  
  growth_inputs <- c("age_at_measurement","sex", "height", "weight")
  
  for (input in growth_inputs) {
    inputs <- base_inputs
    inputs[[input]] <- inputs[[input]][1]
    
    expect_error(do.call(peld_cr, inputs),
      "CDC growth inputs must match")}
})

test_that("PELD-Cr rejects incompatible dialysis vector lengths", {
  
  for (dialysis in list(logical(0),
    c(TRUE, FALSE), rep(FALSE, 4))) {
    
    expect_error(
      peld_cr(age_years = rep(2, 3), albumin = rep(1.5, 3),
        bilirubin = rep(5, 3), inr = rep(2.5, 3), creatinine = rep(0.6, 3),
        growth_z = rep(-3, 3), dialysis = dialysis),
      "dialysis")}
})

test_that("PELD-Cr rejects empty required patient-level inputs", {
  
  base_inputs <- list(age_years = 2, albumin = 1.5,
    bilirubin = 5, inr = 2.5, creatinine = 0.6, growth_z = -3)
  
  required_inputs <- c("age_years", "albumin",
    "bilirubin", "inr", "creatinine")
  
  for (input in required_inputs) {
    
    inputs <- base_inputs
    inputs[[input]] <- numeric(0)
    
    expect_error(do.call(peld_cr, inputs),
      "at least one observation")}
})

test_that("PELD-Cr rejects simultaneous CDC and external growth inputs", {
  
  expect_error(
    peld_cr(age_years = 2, albumin = 1.5, bilirubin = 5, inr = 2.5,
      creatinine = 0.6, age_at_measurement = 24, sex = "female",
      height = 75, weight = 10, growth_z = -3),
    "not both")
})

test_that("PELD-Cr requires a complete growth input pathway", {
  
  base_inputs <- list(age_years = 2, albumin = 1.5,
    bilirubin = 5, inr = 2.5, creatinine = 0.6,
    age_at_measurement = 24, sex = "female",
    height = 75, weight = 10)
  
  growth_inputs <- c("age_at_measurement", "sex", "height", "weight")
  
  for (input in growth_inputs) {
    inputs <- base_inputs
    inputs[[input]] <- NULL
    
    expect_error(do.call(peld_cr, inputs),
      "Provide either growth_z")
  }
  
  inputs <- base_inputs
  inputs[growth_inputs] <- NULL
  
  expect_error(do.call(peld_cr, inputs),
    "Provide either growth_z")
})

test_that("PELD-Cr rejects invalid external growth z-scores", {
  
  base_inputs <- list(age_years = rep(2, 3),
                      albumin = rep(1.5, 3),
                      bilirubin = rep(5, 3),
                      inr = rep(2.5, 3),
                      creatinine = rep(0.6, 3))
  
  invalid_growth <- list(rep("invalid", 3),
    rep(TRUE, 3), numeric(0), -3, c(-3, -4),
    rep(-3, 4), c(-3, Inf, -4), c(-3, -Inf, -4))
  
  for (value in invalid_growth) {
    inputs <- base_inputs
    inputs$growth_z <- value
    
    expect_error(do.call(peld_cr, inputs),
      "growth_z")}
})

test_that("PELD-Cr rejects invalid ages at anthropometric measurement", {
  
  for (age in c(-1, 144, 145, Inf)) {
    
    expect_error(
      peld_cr(age_years = 2, albumin = 1.5, bilirubin = 5,
        inr = 2.5, creatinine = 0.6, age_at_measurement = age,
        sex = "female", height = 75, weight = 10),
      "age_at_measurement")}
})

