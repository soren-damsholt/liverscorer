test_that("apri is calculated correctly", {
  calculated <- apri(ast = 80, ast_uln = 40, platelets = 250)
  
  expected <- 0.8
  
  expect_equal(calculated, expected)
})