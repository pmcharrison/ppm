context("test-noise")

test_that("misc", {
  x <- new_ppm_decay(20, noise = 0.1, buffer_weight = 1)
  x$insert(1:3, 0L, 0L, FALSE)
  y <- vapply(1:1e5, function(i) {
    x$get_weight(1:3, 0, 0, TRUE)
  }, numeric(1))
  
  expect_equal(
    mean(y), 
    1 + x$noise_mean,
    tolerance = 1e-2
  )
  
})


# 
# # We didn't go ahead with this type of noise.
# test_that("misc", {
#   x <- new_ppm_decay(20, noise = 0.1)
#   for (i in seq_len(100000)) {
#     x$insert(1:3, 0L, 0L, FALSE)
#   }
#   df <- x$as_tibble()
#   df$n <- vapply(df$pos, length, integer(1))
#   df$char <- vapply(df$n_gram, function(x) paste(letters[x], collapse = ""), character(1))
#   df$dist <- as.integer(adist("abc", df$char))
#   
#   prop_exact <- sum(df$n[df$dist == 0]) / sum(df$n)
#   prop_1_diff <- sum(df$n[df$dist == 1]) / sum(df$n)
#   prop_2_diff <- sum(df$n[df$dist == 2]) / sum(df$n)
#   prop_3_diff <- sum(df$n[df$dist == 3]) / sum(df$n)
#   
#   expect_equal(prop_exact, (0.9 + 0.1 / 20) ^ 3, tolerance = 0.01)
#   expect_equal(prop_1_diff, 3 * ((0.1 * 19 / 20) * (0.9 + 0.1 / 20) ^ 2), tolerance = 0.01)
#   expect_equal(prop_2_diff, 3 * ((0.1 * 19 / 20) ^ 2 * (0.9 + 0.1 / 20)), tolerance = 0.01)
#   expect_equal(prop_3_diff, (0.1 * 19 / 20) ^ 3, tolerance = 0.01)
# 
# })
