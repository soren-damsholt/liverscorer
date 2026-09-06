test_that("lfi calculates expected values from three grip measurements", {
  result <- lfi(
    grip1 = 25,
    grip2 = 26,
    grip3 = 27,
    time_to_stand = 10,
    side_balance = 10,
    semi_tandem = 10,
    tandem = 10,
    sex = "male")
  
  expect_equal(result, 3.81)
})

test_that("lfi gives same result using precalculated mean grip", {
  result_trials <- lfi(
    grip1 = 25,
    grip2 = 26,
    grip3 = 27,
    time_to_stand = 10,
    side_balance = 10,
    semi_tandem = 10,
    tandem = 10,
    sex = "male")
  
  result_mean <- lfi(
    grip_mean = 26,
    time_to_stand = 10,
    side_balance = 10,
    semi_tandem = 10,
    tandem = 10,
    sex = "male",
    grip_option = TRUE)
  
  expect_equal(result_trials, result_mean)
})

test_that("lfi applies sex-specific grip adjustment", {
  male <- lfi(
    grip_mean = 26,
    time_to_stand = 10,
    side_balance = 10,
    semi_tandem = 10,
    tandem = 10,
    sex = "male",
    grip_option = TRUE)
  
  female <- lfi(
    grip_mean = 26,
    time_to_stand = 10,
    side_balance = 10,
    semi_tandem = 10,
    tandem = 10,
    sex = "female",
    grip_option = TRUE)
  
  expect_false(identical(male, female))
})

test_that("lfi is vectorized", {
  result <- lfi(
    grip1 = c(25, 30),
    grip2 = c(26, 31),
    grip3 = c(27, 32),
    time_to_stand = c(10, 8),
    side_balance = c(10, 10),
    semi_tandem = c(10, 10),
    tandem = c(10, 9),
    sex = c("male", "female"))
  
  expect_length(result, 2)
  expect_true(all(is.numeric(result)))
})

test_that("missing patient data produces NA only for that observation", {
  result <- lfi(
    grip1 = c(25, 30),
    grip2 = c(26, 31),
    grip3 = c(27, 32),
    time_to_stand = c(10, NA),
    side_balance = c(10, 10),
    semi_tandem = c(10, 10),
    tandem = c(10, 10),
    sex = c("male", "female")
  )
  
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("invalid grip_option errors", {
  expect_error(
    lfi(
      25, 26, 27, 10, 10, 10, 10,
      sex = "male",
      grip_option = NA
    ),
    "grip_option must be TRUE or FALSE")
})

test_that("invalid sex errors", {
  expect_error(
    lfi(
      25, 26, 27, 10, 10, 10, 10,
      sex = "other"
    ),
    "sex")
})

test_that("non-numeric clinical input errors", {
  expect_error(
    lfi(
      grip1 = "25",
      grip2 = 26,
      grip3 = 27,
      time_to_stand = 10,
      side_balance = 10,
      semi_tandem = 10,
      tandem = 10,
      sex = "male"
    ),
    "must be numeric"
  )
})

test_that("balance measurements must be between 0 and 10", {
  expect_error(
    lfi(25, 26, 27, 10, 11, 10, 10, sex = "male"),
    "side_balance")
  
  expect_error(
    lfi(25, 26, 27, 10, 10, -1, 10, sex = "male"),
    "semi_tandem")
})

test_that("chair stand time must be between 0 and 60", {
  expect_error(
    lfi(25, 26, 27, 61, 10, 10, 10, sex = "male"),
    "time_to_stand"
  )
  
  expect_error(
    lfi(25, 26, 27, -1, 10, 10, 10, sex = "male"),
    "time_to_stand"
  )
})

test_that("negative grip strength errors", {
  expect_error(
    lfi(
      grip_mean = -1,
      time_to_stand = 10,
      side_balance = 10,
      semi_tandem = 10,
      tandem = 10,
      sex = "male",
      grip_option = TRUE
    ),
    "Grip strength cannot be negative"
  )
})

test_that("patient-level inputs must have equal lengths", {
  expect_error(
    lfi(
      grip1 = c(25, 30),
      grip2 = c(26, 31),
      grip3 = c(27, 32),
      time_to_stand = 10,
      side_balance = c(10, 10),
      semi_tandem = c(10, 10),
      tandem = c(10, 10),
      sex = c("male", "female")
    ),
    "same length")
})

test_that("zero chair stand time is treated as zero stands per second", {
  result <- lfi(
    grip_mean = 26,
    time_to_stand = 0,
    side_balance = 10,
    semi_tandem = 10,
    tandem = 10,
    sex = "male",
    grip_option = TRUE
  )
  expect_true(is.finite(result))
})

test_that("digits controls rounding", {
  result_2 <- lfi(
    25, 26, 27, 10, 10, 10, 10,
    sex = "male",
    digits = 2
  )
  
  result_3 <- lfi(
    25, 26, 27, 10, 10, 10, 10,
    sex = "male",
    digits = 3
  )
  expect_equal(result_2, round(result_3, 2))
})