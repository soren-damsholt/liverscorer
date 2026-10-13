test_that("FIB-4 calculates scalar and vector scores correctly", {
  expect_equal(fib4(45, 45, 145, 45), 2.08)
  expect_equal(
    fib4(c(45, 50, 60), c(64, 20, 230), c(120, 80, 86), c(80, 14, 180)),
    c(2.68, 3.34, 11.96)
  )
  expect_equal(fib4(45, 45, 145, 45, digits = 4),
               round(45 * 45 / (145 * sqrt(45)), 4))
  expect_equal(fib4(45, 45, 145, 45, digits = 0), 2)
})

test_that("FIB-4 warns at both age boundaries", {
  for (age in c(20, 35, 65, 80)) {
    expect_warning(fib4(age, 45, 145, 45), "Use with caution")
  }
  expect_no_warning(fib4(c(36, 64), c(45, 45), c(145, 145), c(45, 45)))
})

test_that("fib4 validates every patient input type and length", {
  baseline <- list(age = 45, asat = 45, platelets = 145, alat = 45)
  for (name in names(baseline)) {
    for (value in list("1", TRUE, factor("1"))) {
      inputs <- baseline
      inputs[[name]] <- value
      expect_error(do.call(fib4, inputs), "numeric", info = name)
    }
    inputs <- baseline
    inputs[[name]] <- numeric(0)
    expect_error(do.call(fib4, inputs), "at least one observation", info = name)
    inputs[[name]] <- rep(baseline[[name]], 2)
    expect_error(do.call(fib4, inputs), "same length", info = name)
  }
})

test_that("fib4 propagates missing data only to the affected patient", {
  baseline <- list(age = 45, asat = 45, platelets = 145, alat = 45)
  expected <- do.call(fib4, baseline)
  for (name in names(baseline)) {
    inputs <- lapply(baseline, rep, times = 3)
    inputs[[name]][2] <- NA_real_
    expect_equal(do.call(fib4, inputs), c(expected, NA_real_, expected), info = name)
    inputs <- baseline
    inputs[[name]] <- NA
    expect_true(is.na(do.call(fib4, inputs)), info = name)
  }
  inputs <- lapply(baseline, function(x) rep(NA, 2))
  expect_true(all(is.na(do.call(fib4, inputs))))
})

test_that("fib4 rejects invalid rounding settings", {
  for (digits in list(-1, 1.5, NA, "2", TRUE, numeric(0), c(1, 2))) {
    expect_error(fib4(age = 45, asat = 45, platelets = 145, alat = 45, digits = digits),
                 "digits must be a single non-negative integer")
  }
})

test_that("fib4 rejects nonpositive measurements", {
  baseline <- list(age = 45, asat = 45, platelets = 145, alat = 45)
  for (name in names(baseline)) {
    for (value in c(0, -1)) {
      inputs <- baseline
      inputs[[name]] <- value
      expect_error(do.call(fib4, inputs), "negative or zero|greater than 0", info = name)
    }
  }
})
