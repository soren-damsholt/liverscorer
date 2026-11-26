test_that("CIRAS lookup contains all unique combinations", {
  expect_equal(nrow(.ciras_lookup), 24L)

  keys <- paste(
    .ciras_lookup$ascites_discomfort,
    .ciras_lookup$ascites,
    .ciras_lookup$natrium,
    sep = "|"
  )

  expect_equal(length(unique(keys)), 24L)
  expect_false(anyNA(.ciras_lookup$score))
})

test_that("CIRAS lookup scores all published combinations correctly", {
  discomfort <- rep(
    c(25, 25, 25, 75, 75, 75),
    times = 4
  )

  ascites_accumulation <- rep(
    c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    times = 4
  )

  diuretics <- rep(
    c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    times = 4
  )

  sodium <- rep(
    c(140, 139, 132, 124),
    each = 6
  )

  expected <- c(
    0, 1, 2,
    1, 2, 3,
    1, 2, 3,
    2, 3, 4,
    2, 3, 4,
    3, 4, 5,
    4, 5, 6,
    5, 5, 6
  )

  calculated <- ciras(
    ascites_discomfort = discomfort,
    ascites_accumulation = ascites_accumulation,
    diuretics = diuretics,
    sodium = sodium
  )

  expect_equal(calculated, expected)
})

test_that("CIRAS discomfort threshold is handled correctly", {
  low <- ciras(
    ascites_discomfort = 50,
    ascites_accumulation = FALSE,
    diuretics = FALSE,
    sodium = 140
  )

  high <- ciras(
    ascites_discomfort = 50.01,
    ascites_accumulation = FALSE,
    diuretics = FALSE,
    sodium = 140
  )

  expect_equal(low, 0)
  expect_equal(high, 1)
})

test_that("CIRAS sodium thresholds are handled correctly", {
  calculated <- ciras(
    ascites_discomfort = rep(25, 7),
    ascites_accumulation = rep(FALSE, 7),
    diuretics = rep(FALSE, 7),
    sodium = c(140, 139.99, 133, 132.99, 125, 124.99, 100)
  )

  expected <- c(0, 1, 1, 2, 2, 4, 4)

  expect_equal(calculated, expected)
})

test_that("CIRAS ascites groups are classified correctly", {
  calculated <- ciras(
    ascites_discomfort = c(25, 25, 25),
    ascites_accumulation = c(FALSE, TRUE, TRUE),
    diuretics = c(FALSE, TRUE, FALSE),
    sodium = c(140, 140, 140)
  )

  expect_equal(calculated, c(0, 1, 2))
})

test_that("diuretic status is irrelevant when ascites is not accumulating", {
  expect_no_warning(
    calculated <- ciras(
      ascites_discomfort = 25,
      ascites_accumulation = FALSE,
      diuretics = NA,
      sodium = 140
    )
  )

  expect_equal(calculated, 0)
})

test_that("missing diuretic status gives NA when ascites is accumulating", {
  expect_warning(
    calculated <- ciras(
      ascites_discomfort = 25,
      ascites_accumulation = TRUE,
      diuretics = NA,
      sodium = 140
    ),
    "Missing inputs"
  )

  expect_true(is.na(calculated))
})

test_that("CIRAS propagates missingness only to affected observations", {
  expect_warning(
    calculated <- ciras(
      ascites_discomfort = c(25, NA, 75),
      ascites_accumulation = c(FALSE, FALSE, TRUE),
      diuretics = c(FALSE, FALSE, FALSE),
      sodium = c(140, 140, 124)
    ),
    "Missing inputs"
  )

  expect_equal(calculated, c(0, NA_integer_, 6))
})

test_that("CIRAS rejects invalid numeric inputs", {
  expect_error(
    ciras("wrong", FALSE, FALSE, 140),
    "numeric"
  )

  expect_error(
    ciras(25, FALSE, FALSE, "wrong"),
    "numeric"
  )
})

test_that("CIRAS rejects invalid logical inputs", {
  expect_error(
    ciras(25, 1, FALSE, 140),
    "TRUE or FALSE"
  )

  expect_error(
    ciras(25, FALSE, 1, 140),
    "TRUE or FALSE"
  )
})

test_that("CIRAS rejects impossible values", {
  expect_error(
    ciras(-1, FALSE, FALSE, 140),
    "between 0 and 100"
  )

  expect_error(
    ciras(101, FALSE, FALSE, 140),
    "between 0 and 100"
  )

  expect_error(
    ciras(25, FALSE, FALSE, 0),
    "greater than 0"
  )
})

test_that("CIRAS patient-level inputs must have equal lengths", {
  expect_error(
    ciras(
      ascites_discomfort = c(25, 75),
      ascites_accumulation = FALSE,
      diuretics = FALSE,
      sodium = c(140, 124)
    ),
    "same length"
  )
})

test_that("CIRAS rejects empty patient-level inputs", {
  expect_error(
    ciras(
      ascites_discomfort = numeric(0),
      ascites_accumulation = logical(0),
      diuretics = logical(0),
      sodium = numeric(0)
    ),
    "at least one observation"
  )
})
