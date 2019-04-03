context("test-find_order")

test_that("buffer models", {
  # With an buffer length of 2 seconds,
  # and with events every 1 second,
  # the maximum model order should be 2.
  new_ppm_decay(alphabet_size = 10, 
                buffer_length_time = 2) %>% 
    model_seq(rep(1, times = 10),
              time = 1:10) %>% 
    `$`(model_order) %>% 
    max() %>% 
    expect_equal(2)
  
  # Likewise for a 3-second buffer:
  new_ppm_decay(alphabet_size = 10, 
                buffer_length_time = 3) %>% 
    model_seq(rep(1, times = 10),
              time = 1:10) %>% 
    `$`(model_order) %>% 
    max() %>% 
    expect_equal(3)
  
  # A 1.9-second buffer should yield an order bound of 1.
  new_ppm_decay(alphabet_size = 10, 
                buffer_length_time = 1.9) %>% 
    model_seq(rep(1, times = 10),
              time = 1:10) %>% 
    `$`(model_order) %>% 
    max() %>% 
    expect_equal(1)
  
  # Now multiplying everything by 10:
  new_ppm_decay(alphabet_size = 10, 
                buffer_length_time = 20) %>% 
    model_seq(rep(1, times = 10),
              time = seq(from = 10, to = 100, by = 10)) %>% 
    `$`(model_order) %>% 
    max() %>% 
    expect_equal(2)
})
