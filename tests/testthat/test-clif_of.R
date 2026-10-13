test_that("CLIF-C OF scores each organ at its thresholds", {
  baseline <- list(bilirubin = 1, creatinine = 1, inr = 1,
                   encephalopathy = 0, map = 80, oxygen_ratio = 400)
  expect_equal(do.call(clif_of, baseline), 6)
  cases <- list(
    bilirubin = list(values = c(5.99, 6, 11.99, 12), scores = c(6, 7, 7, 8)),
    creatinine = list(values = c(1.99, 2, 3.49, 3.5), scores = c(6, 7, 7, 8)),
    inr = list(values = c(1.99, 2, 2.49, 2.5), scores = c(6, 7, 7, 8)),
    encephalopathy = list(values = 0:4, scores = c(6, 7, 7, 8, 8)),
    map = list(values = c(69.99, 70, 70.01), scores = c(7, 6, 6)),
    oxygen_ratio = list(values = c(199, 200, 200.01, 300, 300.01),
                        scores = c(8, 8, 7, 7, 6))
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    args <- lapply(baseline, rep, times = length(case$values))
    args[[name]] <- case$values
    expect_equal(do.call(clif_of, args), case$scores, info = name)
  }
  expect_equal(clif_of(12, 3.5, 2.5, 4, 60, 200, vasopressors = TRUE), 18)
})

test_that("CLIF-C OF uses SpO2-specific respiratory thresholds", {
  expect_equal(clif_of(rep(1, 5), rep(1, 5), rep(1, 5), rep(0, 5), rep(80, 5),
                       c(213, 214, 214.01, 357, 357.01), oxygen_ratio_type = "spo2"),
               c(8, 8, 7, 7, 6))
})

test_that("CLIF-C OF treatments override the corresponding measurements", {
  baseline <- list(bilirubin = c(1, 1), creatinine = c(1, 1), inr = c(1, 1),
                   encephalopathy = c(0, 0), map = c(80, 80), oxygen_ratio = c(400, 400))
  for (setting in c("rrt", "vasopressors", "mechanical_ventilation")) {
    args <- baseline
    args[[setting]] <- c(FALSE, TRUE)
    expect_equal(do.call(clif_of, args), c(6, 8), info = setting)
    args[[setting]] <- TRUE
    expect_equal(do.call(clif_of, args), c(8, 8), info = setting)
  }
  expect_equal(clif_of(1, 1, 1, 0, 80, 400, rrt = TRUE,
                       vasopressors = TRUE, mechanical_ventilation = TRUE), 12)
  expect_equal(clif_of(1, 1, 1, 0, 80, 400, oxygen_ratio_type = "spo2",
                       mechanical_ventilation = TRUE), 8)
})

test_that("CLIF-C OF converts SI units with default and custom factors", {
  expect_equal(clif_of(120, 80, 2, 2, 70, 340, units = "SI"),
               clif_of(120 * 0.0584, 80 * 0.0113, 2, 2, 70, 340))
  expect_equal(clif_of(60, 20, 2, 2, 70, 340, units = "SI",
                       bilirubin_conversion_factor = 0.1,
                       creatinine_conversion_factor = 0.1),
               clif_of(6, 2, 2, 2, 70, 340))
})

test_that("CLIF-C OF rejects invalid settings and West-Haven grades", {
  baseline <- list(bilirubin = 1, creatinine = 1, inr = 1,
                   encephalopathy = 0, map = 80, oxygen_ratio = 400)
  for (setting in c("units", "oxygen_ratio_type")) {
    for (value in list("other", NA, character(0), c("US", "SI"))) {
      args <- baseline
      args[[setting]] <- value
      expect_error(do.call(clif_of, args), paste0(setting, " must be one of"))
    }
  }
  for (grade in c(-1, 0.5, 5)) {
    args <- baseline
    args$encephalopathy <- grade
    expect_error(do.call(clif_of, args), "integer between 0 and 4")
  }
  for (setting in c("rrt", "vasopressors", "mechanical_ventilation")) {
    for (value in list(1, "TRUE", NA, logical(0), c(TRUE, FALSE))) {
      args <- baseline
      args[[setting]] <- value
      expect_error(do.call(clif_of, args), "must be TRUE or FALSE|must have length 1")
    }
  }
})

test_that("clif_of validates every patient input type and length", {
  baseline <- list(bilirubin = 1, creatinine = 1, inr = 1, encephalopathy = 0, map = 80, oxygen_ratio = 400)
  for (name in names(baseline)) {
    for (value in list("1", TRUE, factor("1"))) {
      inputs <- baseline
      inputs[[name]] <- value
      expect_error(do.call(clif_of, inputs), "numeric", info = name)
    }
    inputs <- baseline
    inputs[[name]] <- numeric(0)
    expect_error(do.call(clif_of, inputs), "at least one observation", info = name)
    inputs[[name]] <- rep(baseline[[name]], 2)
    expect_error(do.call(clif_of, inputs), "same length", info = name)
  }
})

test_that("clif_of propagates missing data only to the affected patient", {
  baseline <- list(bilirubin = 1, creatinine = 1, inr = 1, encephalopathy = 0, map = 80, oxygen_ratio = 400)
  expected <- do.call(clif_of, baseline)
  for (name in names(baseline)) {
    inputs <- lapply(baseline, rep, times = 3)
    inputs[[name]][2] <- NA_real_
    expect_equal(do.call(clif_of, inputs), c(expected, NA_real_, expected), info = name)
    inputs <- baseline
    inputs[[name]] <- NA
    expect_true(is.na(do.call(clif_of, inputs)), info = name)
  }
  inputs <- lapply(baseline, function(x) rep(NA, 2))
  expect_true(all(is.na(do.call(clif_of, inputs))))
})

test_that("clif_of rejects nonpositive measurements", {
  baseline <- list(bilirubin = 1, creatinine = 1, inr = 1, encephalopathy = 0, map = 80, oxygen_ratio = 400)
  for (name in setdiff(names(baseline), "encephalopathy")) {
    for (value in c(0, -1)) {
      inputs <- baseline
      inputs[[name]] <- value
      expect_error(do.call(clif_of, inputs), "negative or zero|greater than 0", info = name)
    }
  }
})
