test_that("apri is calculated correctly", {
  calculated <- apri(ast = 80, ast_uln = 40, platelets = 250)

  expected <- 0.8

  expect_equal(calculated, expected)
})

test_that("apri is vectorized", {
  calculated <- apri(
    ast = c(80, 100),
    ast_uln = c(40, 50),
    platelets = c(250, 200)
  )
  expected <- c(
    80 / 40 / 250 * 100,
    100 / 50 / 200 * 100
  )

  expect_equal(calculated, expected)
})

test_that("scalar AST ULN applies to all observations", {
  calculated <- apri(
    ast = c(80, 100),
    ast_uln = 40,
    platelets = c(250, 200)
  )

  expected <- c(0.8, 1.25)

  expect_equal(calculated, expected)
})

test_that("missing inputs produce NA and warning", {
  expect_warning(
    calculated <- apri(
      ast = c(80, NA, 100),
      ast_uln = 40,
      platelets = c(250, 250, 200)
    ),
    "Missing inputs"
  )

  expected <- c(0.8, NA_real_, 1.25)

  expect_equal(calculated, expected)
})

test_that("invalid numeric values are rejected", {
  expect_error(
    apri(ast = -1, ast_uln = 40, platelets = 250)
  )

  expect_error(
    apri(ast = 80, ast_uln = 0, platelets = 250)
  )

  expect_error(
    apri(ast = 80, ast_uln = -1, platelets = 250)
  )

  expect_error(
    apri(ast = 80, ast_uln = 40, platelets = 0)
  )

  expect_error(
    apri(ast = 80, ast_uln = 40, platelets = -1)
  )
})

test_that("invalid types produces error", {
  baseline <- list(
    ast = 80,
    ast_uln = 40,
    platelets = 250
  )

  for (name in names(baseline)) {
    inputs <- baseline
    inputs[[name]] <- "wrong"

    expect_error(
      do.call(apri, inputs),
      "numeric",
      info = name
    )
  }
})

test_that("patient-level inputs must have equal lengths", {
  expect_error(
    apri(
      ast = c(80, 100),
      ast_uln = 40,
      platelets = 250
    ),
    "same length"
  )
})

test_that("AST ULN must be scalar or match patient-level length", {
  expect_error(
    apri(
      ast = c(80, 100, 120),
      ast_uln = c(40, 45),
      platelets = c(250, 250, 250)
    ),
    "length 1 or match"
  )
})

test_that("empty patient-level inputs are rejected", {
  expect_error(
    apri(
      ast = numeric(0),
      ast_uln = 40,
      platelets = numeric(0)
    ),
    "at least one observation"
  )
})
