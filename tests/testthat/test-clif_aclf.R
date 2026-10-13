test_that("CLIF-C ACLF calculates from independently specified organ scores", {
  # Organ subscores: (1,1,1,1,1,1), (2,2,2,2,1,1), (3,3,3,3,3,3).
  age <- c(50, 60, 70)
  wbc <- c(10, 20, 30)
  for (digits in c(0, 2, 4)) {
    expected <- round(10 * (0.33 * c(6, 10, 18) + 0.04 * age +
                             0.63 * log(wbc) - 2), digits)
    expect_equal(clif_aclf(age, wbc, c(1, 6, 12), c(1, 2, 3.5), c(0, 2, 4),
                           c(1, 2, 2.5), c(80, 70, 60), c(400, 340, 200),
                           vasopressors = c(FALSE, FALSE, TRUE), digits = digits),
                 expected)
  }
  expect_equal(clif_aclf(50, 10, 1, 1, 0, 1, 80, 400), 34)
})

test_that("CLIF-C ACLF bounds the final score between 0 and 100", {
  expect_equal(clif_aclf(1, 0.1, 1, 1, 0, 1, 80, 400), 0)
  expect_equal(clif_aclf(100, 1000, 12, 4, 4, 3, 60, 100,
                         vasopressors = TRUE), 100)
})

test_that("CLIF-C ACLF forwards units, oxygen type and treatment settings", {
  expect_equal(clif_aclf(50, 10, 120, 80, 2, 2, 70, 340, units = "SI", digits = 4),
               clif_aclf(50, 10, 120 * 0.0584, 80 * 0.0113, 2, 2, 70, 340, digits = 4))
  expect_equal(clif_aclf(50, 10, 1, 1, 0, 1, 80, 350, oxygen_ratio_type = "spo2"),
               round(10 * (0.33 * 7 + 0.04 * 50 + 0.63 * log(10) - 2)))
  baseline <- list(age = c(50, 50), wbc = c(10, 10), bilirubin = c(1, 1),
                   creatinine = c(1, 1), encephalopathy = c(0, 0), inr = c(1, 1),
                   map = c(80, 80), oxygen_ratio = c(400, 400), digits = 4)
  for (setting in c("rrt", "vasopressors", "mechanical_ventilation")) {
    args <- baseline
    args[[setting]] <- c(FALSE, TRUE)
    expect_equal(do.call(clif_aclf, args),
      round(10 * (0.33 * c(6, 8) + 0.04 * 50 + 0.63 * log(10) - 2), 4))
    args[[setting]] <- TRUE
    expect_equal(do.call(clif_aclf, args),
      rep(round(10 * (0.33 * 8 + 0.04 * 50 + 0.63 * log(10) - 2), 4), 2))
  }
})

test_that("CLIF-C ACLF rejects nonpositive age and WBC", {
  for (name in c("age", "wbc")) {
    for (value in c(0, -1)) {
      args <- list(age = 50, wbc = 10, bilirubin = 1, creatinine = 1,
                   encephalopathy = 0, inr = 1, map = 80, oxygen_ratio = 400)
      args[[name]] <- value
      expect_error(do.call(clif_aclf, args), paste0(name, " must be greater than 0"))
    }
  }
})

test_that("CLIF-C ACLF propagates organ input validation", {
  expect_error(clif_aclf(50, 10, 1, 1, 5, 1, 80, 400), "integer between 0 and 4")
  expect_error(clif_aclf(50, 10, 1, 1, 0, 1, 80, 400, units = "si"), "units must be one of")
  expect_error(clif_aclf(50, 10, 1, 1, 0, 1, 80, 400, oxygen_ratio_type = "other"),
               "oxygen_ratio_type must be one of")
  for (setting in c("rrt", "vasopressors", "mechanical_ventilation")) {
    args <- list(age = 50, wbc = 10, bilirubin = 1, creatinine = 1,
                 encephalopathy = 0, inr = 1, map = 80, oxygen_ratio = 400)
    args[[setting]] <- NA
    expect_error(do.call(clif_aclf, args), "must be TRUE or FALSE")
  }
})

test_that("clif_aclf validates every patient input type and length", {
  baseline <- list(age = 50, wbc = 10, bilirubin = 1, creatinine = 1, encephalopathy = 0, inr = 1, map = 80, oxygen_ratio = 400)
  for (name in names(baseline)) {
    for (value in list("1", TRUE, factor("1"))) {
      inputs <- baseline
      inputs[[name]] <- value
      expect_error(do.call(clif_aclf, inputs), "numeric", info = name)
    }
    inputs <- baseline
    inputs[[name]] <- numeric(0)
    expect_error(do.call(clif_aclf, inputs), "at least one observation", info = name)
    inputs[[name]] <- rep(baseline[[name]], 2)
    expect_error(do.call(clif_aclf, inputs), "same length", info = name)
  }
})

test_that("clif_aclf propagates missing data only to the affected patient", {
  baseline <- list(age = 50, wbc = 10, bilirubin = 1, creatinine = 1, encephalopathy = 0, inr = 1, map = 80, oxygen_ratio = 400)
  expected <- do.call(clif_aclf, baseline)
  for (name in names(baseline)) {
    inputs <- lapply(baseline, rep, times = 3)
    inputs[[name]][2] <- NA_real_
    expect_equal(do.call(clif_aclf, inputs), c(expected, NA_real_, expected), info = name)
    inputs <- baseline
    inputs[[name]] <- NA
    expect_true(is.na(do.call(clif_aclf, inputs)), info = name)
  }
  inputs <- lapply(baseline, function(x) rep(NA, 2))
  expect_true(all(is.na(do.call(clif_aclf, inputs))))
})

test_that("clif_aclf rejects invalid rounding settings", {
  for (digits in list(-1, 1.5, NA, "2", TRUE, numeric(0), c(1, 2))) {
    expect_error(clif_aclf(age = 50, wbc = 10, bilirubin = 1, creatinine = 1,
                           encephalopathy = 0, inr = 1, map = 80,
                           oxygen_ratio = 400, digits = digits),
                 "digits must be a single non-negative integer")
  }
})
test_that("clif_aclf does not recycle patient-level age or WBC", {
  expect_error(
    clif_aclf(
      age = c(50, 60),
      wbc = 10,
      bilirubin = c(1, 1),
      creatinine = c(1, 1),
      encephalopathy = c(0, 0),
      inr = c(1, 1),
      map = c(80, 80),
      oxygen_ratio = c(400, 400)
    ),
    "same length"
  )})