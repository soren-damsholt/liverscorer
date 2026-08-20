testthat::test_that("Testing that meld score calculates correctly, validated against the online calculator at: https://www.mdcalc.com/calc/10437/model-end-stage-liver-disease-meld#evidence", {
  testthat::expect_equal(liverscorer:::meld(creatinine = 120, bilirubin = 80, inr = 2.0, unit = "SI", dialysis = "no"), 23)
  testthat::expect_equal(liverscorer:::meld(creatinine = 120, bilirubin = 80, inr = 2.0, unit = "SI", dialysis = "yes"), 33)
  testthat::expect_equal(liverscorer:::meld(creatinine = 1.2, bilirubin = 5, inr = 2.0, unit = "US", dialysis = "no"), 22)
  testthat::expect_equal(liverscorer:::meld(creatinine = 1.2, bilirubin = 5, inr = 2.0, unit = "US", dialysis = "yes"), 34)
  
})
