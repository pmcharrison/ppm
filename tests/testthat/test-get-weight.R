context("test-get-weight")

test_that("misc", {
  f <- function(seq, 
                n_gram,
                pos, time,
                data_time, 
                alphabet_size = 100,
                noise = 0,
                ...) {
    mod <- new_ppm_decay(alphabet_size = alphabet_size, noise = noise, ...)
    model_seq(mod, seq, time = data_time, 
              train = TRUE,
              predict = FALSE, 
              zero_indexed = TRUE)
    get_weight(mod, n_gram, pos = pos, time = time, update_excluded = FALSE)
  }
  
  decay_exp <- function(time_elapsed, half_life, start, end) {
    lambda <- log(2) / half_life
    end + (start - end) * exp(- lambda * time_elapsed)
  }
  
  ## Item buffers
  # Buffer = 10 - everything at full stm_rate
  f(seq = rep(1, times = 9),
    n_gram = 1,
    pos = 10, time = 10,
    data_time = 1:9, 
    buffer_length_time = 999999,
    buffer_length_items = 10,
    buffer_weight = 1,
    stm_half_life = 0.000000001,
    stm_weight = 1,
    ltm_weight = 0,
    noise = 0) %>% expect_equal(9)
  
  # No more than 10 cases can be counted
  f(seq = rep(1, times = 15),
    n_gram = 1,
    pos = 16, time = 16,
    data_time = 1:15, 
    buffer_length_time = 999999,
    buffer_length_items = 10,
    buffer_weight = 1,
    stm_half_life = 0.000000001,
    stm_weight = 1,
    ltm_weight = 0,
    noise = 0) %>% expect_equal(10)
  
  # Now set a non-zero ltm_rate
  f(seq = rep(1, times = 15),
    n_gram = 1,
    pos = 16, time = 16,
    data_time = 1:15, 
    buffer_length_time = 999999,
    buffer_length_items = 10,
    buffer_weight = 1,
    stm_half_life = 0.000000001,
    stm_weight = 1,
    ltm_weight = 0.1,
    noise = 0) %>% expect_equal(10 + 0.5)
  
  # Now to distinguish time from position,
  # we need to set a non-zero half-life.
  
  # Nothing within the buffer decays
  f(seq = rep(1, times = 10),
    n_gram = 1,
    pos = 10, time = 10,
    data_time = 1:10, 
    buffer_length_time = 999999,
    buffer_length_items = 10,
    buffer_weight = 1,
    stm_half_life = 1,
    stm_weight = 1,
    ltm_weight = 0,
    noise = 0) %>% expect_equal(10)

  # Past the buffer, we decay with a half-life of 1
  f(seq = rep(1, times = 11),
    n_gram = 1,
    pos = 12, time = 12,
    data_time = 1:11, 
    buffer_length_time = 999999,
    buffer_length_items = 10,
    buffer_weight = 1,
    stm_half_life = 1,
    stm_weight = 1,
    ltm_weight = 0,
    noise = 0) %>% expect_equal(10 + 0.5)
  
  f(seq = rep(1, times = 11),
    n_gram = 1,
    pos = 12, time = 13,
    data_time = 1:11, 
    buffer_length_time = 999999,
    buffer_length_items = 10,
    buffer_weight = 1,
    stm_half_life = 1,
    stm_weight = 1,
    ltm_weight = 0,
    noise = 0) %>% expect_equal(10 + 0.25)

  ## Time buffers
  f(seq = rep(1, times = 10),
    n_gram = 1,
    pos = 10, time = 10,
    data_time = seq(from = 1, by = 0.5, length.out = 10), 
    buffer_length_time = 7,
    buffer_length_items = 1000,
    buffer_weight = 1,
    stm_half_life = 1,
    stm_weight = 1,
    ltm_weight = 0,
    noise = 0) %>% 
    expect_equal({
      decay_exp(2, 1, 1, 0) +
        decay_exp(1.5, 1, 1, 0) +
        decay_exp(1, 1, 1, 0) +
        decay_exp(0.5, 1, 1, 0) +
        6
    })
  
  ## Buffer rate
  f(seq = rep(1, times = 10),
    n_gram = 1,
    pos = 10, time = 10,
    data_time = seq(from = 1, by = 0.5, length.out = 10), 
    buffer_length_time = 7,
    buffer_length_items = 1000,
    buffer_weight = 0.5,
    stm_half_life = 1,
    stm_weight = 1,
    ltm_weight = 0,
    noise = 0) %>% 
    expect_equal({
      decay_exp(2, 1, 1, 0) +
        decay_exp(1.5, 1, 1, 0) +
        decay_exp(1, 1, 1, 0) +
        decay_exp(0.5, 1, 1, 0) +
        1 + 5 * 0.5
    })
})
