test_that("CLIF-C AD calculates scalar and vector scores correctly", {
  expect_equal(clif_ad(50, 2, 2, 20, 137), 69)
  expect_equal(clif_ad(c(50, 70, 22), c(90, 120, 70), c(2, 1.8, 2.2),
                       c(20, 60, 11), c(137, 122, 140), unit = "SI"),
               c(65, 88, 50))
  for (digits in c(0, 2, 4)) {
    expect_equal(clif_ad(50, 2, 2, 20, 137, digits = digits),
      round(10 * (0.03 * 50 + 0.66 * log(2) + 1.71 * log(2) +
                    0.88 * log(20) - 0.05 * 137 + 8), digits))
  }
})

test_that("CLIF-C AD converts SI creatinine and bounds scores", {
  expect_equal(clif_ad(50, 90, 2, 20, 137, unit = "SI", digits = 4),
               clif_ad(50, 90 * 0.0113, 2, 20, 137, digits = 4))
  expect_equal(clif_ad(20, 0.1, 0.5, 1, 200), 0)
  expect_equal(clif_ad(100, 10, 5, 100, 100), 100)
})

test_that("CLIF-C AD rejects invalid unit settings", {
  for (unit in list("si", NA, character(0), c("SI", "US"))) {
    expect_error(clif_ad(50, 2, 2, 20, 137, unit = unit), "unit must be one of")
  }
})

test_that("clif_ad validates every patient input type and length", {
  baseline <- list(age = 50, creatinine = 2, inr = 2, wbc = 20, sodium = 137)
  for (name in names(baseline)) {
    for (value in list("1", TRUE, factor("1"))) {
      inputs <- baseline
      inputs[[name]] <- value
      expect_error(do.call(clif_ad, inputs), "numeric", info = name)
    }
    inputs <- baseline
    inputs[[name]] <- numeric(0)
    expect_error(do.call(clif_ad, inputs), "at least one observation", info = name)
    inputs[[name]] <- rep(baseline[[name]], 2)
    expect_error(do.call(clif_ad, inputs), "same length", info = name)
  }
})

test_that("clif_ad propagates missing data only to the affected patient", {
  baseline <- list(age = 50, creatinine = 2, inr = 2, wbc = 20, sodium = 137)
  expected <- do.call(clif_ad, baseline)
  for (name in names(baseline)) {
    inputs <- lapply(baseline, rep, times = 3)
    inputs[[name]][2] <- NA_real_
    expect_equal(do.call(clif_ad, inputs), c(expected, NA_real_, expected), info = name)
    inputs <- baseline
    inputs[[name]] <- NA
    expect_true(is.na(do.call(clif_ad, inputs)), info = name)
  }
  inputs <- lapply(baseline, function(x) rep(NA, 2))
  expect_true(all(is.na(do.call(clif_ad, inputs))))
})

test_that("clif_ad rejects invalid rounding settings", {
  for (digits in list(-1, 1.5, NA, "2", TRUE, numeric(0), c(1, 2))) {
    expect_error(clif_ad(age = 50, creatinine = 2, inr = 2, wbc = 20, sodium = 137, digits = digits),
                 "digits must be a single non-negative integer")
  }
})
