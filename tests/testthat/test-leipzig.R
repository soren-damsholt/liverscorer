test_that("leipzig scores correctly", {
  calculated <- leipzig(kayser_fleischer = TRUE, 
                        neurological_symptoms = "absent",
                        ceruloplasmin = 0.2,
                        haemolytic_anemia = FALSE,
                        liver_copper = 0.8,
                        urinary_copper = 1,
                        mutated_chromosomes = 0)
  
  expected <- 5

  expect_equal(calculated, expected)
  
})