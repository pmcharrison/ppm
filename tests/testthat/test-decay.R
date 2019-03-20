context("test-decay")

library(magrittr)

test_decay <- function(seq, time, order_bound = 3, ...) {
  alphabet <- seq %>% unique %>% sort
  m <- new_ppm_decay(alphabet_size = length(alphabet), order_bound = order_bound, ...)
  seq <- factor(seq, levels = alphabet) %>% as.integer() %>% subtract(1L)
  model_seq(m, seq, time, zero_indexed = TRUE)
}

test_that("simple tests", {
  seq <- strsplit(c("abracadabra"), split = "")[[1]]
  
  expect_equal(
    test_decay(seq, 
               time = seq_along(seq), 
               buffer_length_items = 20,  # <----
               buffer_length_time = 100, 
               noise = 0) %>% `$`(information_content),
    test_decay(seq, 
               time = seq_along(seq),
               buffer_length_items = 10, # <----
               buffer_length_time = 100, 
               noise = 0) %>% `$`(information_content)
  )
  
  expect_false(identical(
    test_decay(seq, 
               time = seq_along(seq), 
               buffer_length_items = 20,  # <----
               buffer_length_time = 100, 
               noise = 0) %>% `$`(information_content),
    test_decay(seq, 
               time = seq_along(seq),
               buffer_length_items = 9, # <----
               buffer_length_time = 100, 
               noise = 0) %>% `$`(information_content)
  ))
  
  seq <- strsplit(c("abcabcabdabc"), split = "")[[1]]
  final_dist_1 <- test_decay(
    seq, 
    time = seq_along(seq), 
    buffer_length_items = 20,  # <---- all the sequence fits in the buffer
    buffer_length_time = 100, 
    noise = 0
  ) %>% 
    `$`(distribution) %>% 
    `[[`(12)
  
  final_dist_2 <- test_decay(
    seq, 
    time = seq_along(seq), 
    buffer_length_items = 4,  # <---- buffer of length 4
    buffer_length_time = 100, 
    noise = 0
  ) %>% 
    `$`(distribution) %>% 
    `[[`(12)
  
  expect_true(final_dist_1[3] > final_dist_2[3]) # <--- forgetting about 'c'
  
  x <- test_decay(
    seq, 
    time = seq_along(seq), 
    buffer_length_items = 4,  # <---- buffer of length 4
    buffer_length_time = 100, 
    buffer_weight = 1,
    stm_weight = 0,
    noise = 0
  ) %>% `$`(distribution) 
  
  y <- paste(if (interactive()) "tests/testthat/",
             "data/decay-regression-1.rds", 
             sep = "") %>% 
    readRDS()
  
  expect_equal(x, y)
})

