test_that("ELF uses the correct coefficients for each platform", {
  expect_equal(elf(c(7, 8, 10), c(8, 8, 12), c(9, 35, 29)),
               c(6.36, 7.01, 7.43))
  for (digits in c(0, 2, 4)) {
    expect_equal(elf(7, 8, 9, digits = digits),
      round(2.278 + 0.851 * log(7) + 0.751 * log(8) + 0.394 * log(9), digits))
    expect_equal(elf(7, 8, 9, digits = digits, platform = "centaur_cp"),
      round(2.494 + 0.846 * log(7) + 0.735 * log(8) + 0.391 * log(9), digits))
  }
  expect_equal(elf(1, 1, 1, digits = 3), 2.278)
  expect_equal(elf(1, 1, 1, digits = 3, platform = "centaur_cp"), 2.494)
})

test_that("ELF rejects invalid platform settings", {
  for (platform in list("other", "Centaur_xp", NA, character(0),
                        c("centaur_xp", "centaur_cp"))) {
    expect_error(elf(7, 8, 9, platform = platform), "platform must be one of")
  }
})

test_that("elf validates every patient input type and length", {
  baseline <- list(HA = 7, PIIINP = 8, TIMP = 9)
  for (name in names(baseline)) {
    for (value in list("1", TRUE, factor("1"))) {
      inputs <- baseline
      inputs[[name]] <- value
      expect_error(do.call(elf, inputs), "numeric", info = name)
    }
    inputs <- baseline
    inputs[[name]] <- numeric(0)
    expect_error(do.call(elf, inputs), "at least one observation|same length", info = name)
    inputs[[name]] <- rep(baseline[[name]], 2)
    expect_error(do.call(elf, inputs), "same length", info = name)
  }
})

test_that("elf propagates missing data only to the affected patient", {
  baseline <- list(HA = 7, PIIINP = 8, TIMP = 9)
  expected <- do.call(elf, baseline)
  for (name in names(baseline)) {
    inputs <- lapply(baseline, rep, times = 3)
    inputs[[name]][2] <- NA_real_
    expect_equal(do.call(elf, inputs), c(expected, NA_real_, expected), info = name)
    inputs <- baseline
    inputs[[name]] <- NA
    expect_true(is.na(do.call(elf, inputs)), info = name)
  }
  inputs <- lapply(baseline, function(x) rep(NA, 2))
  expect_true(all(is.na(do.call(elf, inputs))))
})

test_that("elf rejects invalid rounding settings", {
  for (digits in list(-1, 1.5, NA, "2", TRUE, numeric(0), c(1, 2))) {
    expect_error(elf(HA = 7, PIIINP = 8, TIMP = 9, digits = digits),
                 "digits must be a single non-negative integer")
  }
})

test_that("elf rejects nonpositive measurements", {
  baseline <- list(HA = 7, PIIINP = 8, TIMP = 9)
  for (name in setdiff(names(baseline), "encephalopathy")) {
    for (value in c(0, -1)) {
      inputs <- baseline
      inputs[[name]] <- value
      expect_error(do.call(elf, inputs), "negative or zero|greater than 0", info = name)
    }
  }
})
