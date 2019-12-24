test_that("times must be in increasing order", {
  mod <- new_ppm_decay(alphabet_size = 10)
  expect_error(
    model_seq(mod, sample(5, size = 10, replace = TRUE), time = 10:1),
    "decreasing values of time are not permitted"
  )
  
  mod <- new_ppm_decay(alphabet_size = 10)
  model_seq(mod, sample(5, size = 10, replace = TRUE), time = 1:10)
  expect_error(
    model_seq(mod, sample(5, size = 10, replace = TRUE), time = 9:18),
    "a sequence may not begin before the previous sequence finished"
  )
})
