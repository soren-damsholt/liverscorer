test_that("PELD is calculated correctly", {
  result <- peld_cr(age = 10, sex = "female", height = 160, weight = 80,
                 albumin = 26, bilirubin = 50, inr = 1.5, creatinine = 90,
                 dialysis = FALSE, units = "SI")
  
  expected <- 26
  
  expect_equal(result, expected)
  
})