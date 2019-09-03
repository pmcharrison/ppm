context("test-insert")

library(magrittr)

test_that("n-grams should only be inserted if they fit within the temporal buffer", {
  x <- new_ppm_decay(20, 
                     noise = 0.1,
                     buffer_weight = 1, 
                     stm_weight = 1, 
                     ltm_weight = 1,
                     buffer_length_time = 5, 
                     buffer_length_items = 21,
                     order_bound = 20,
                     only_learn_from_buffer = TRUE)
  model_seq(x, 0:19, time = 0:19, predict = FALSE, zero_indexed = TRUE)
  x$as_tibble() %>% `$`(n_gram) %>% vapply(length, integer(1)) %>% max() %>% 
    expect_equal(5)
  
  x <- new_ppm_decay(20, noise = 0.1, 
                     buffer_weight = 1,
                     stm_weight = 1,
                     ltm_weight = 1,
                     buffer_length_time = 5, 
                     buffer_length_items = 21,
                     order_bound = 20,
                     only_learn_from_buffer = TRUE)
  model_seq(x, 0:19, time = seq(from = 0, by = 0.5, length.out = 20), predict = FALSE, zero_indexed = TRUE)
  x$as_tibble() %>% `$`(n_gram) %>% vapply(length, integer(1)) %>% max() %>% 
    expect_equal(10)
})
