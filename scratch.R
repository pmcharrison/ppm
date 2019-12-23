library(tidyverse)
library(ppm)


plot_mod <- function(...) {
  mod <- new_ppm_decay(alphabet_size = 5, ...)
  model_seq(mod,
            seq = 1,
            time = 0, 
            train = TRUE,
            predict = FALSE, 
            zero_indexed = TRUE)
  tibble(x = 1:100,
         y = map_dbl(x, ~ get_weight(mod, 
                                     n_gram = 1,
                                     pos = ., 
                                     time = ., 
                                     update_excluded = FALSE,
                                     zero_indexed = TRUE))) %>% 
    ggplot(aes(x, y)) + 
    geom_line() + 
    scale_x_continuous("Time") +
    scale_y_continuous("Weight", limits = c(0, NA)) +
    theme_classic() + 
    theme(aspect.ratio = 1)
}

plot_mod()
plot_mod(stm_duration = 0,
         ltm_weight = 0.25,
         ltm_half_life = 10,
         ltm_asymptote = 0.05)
