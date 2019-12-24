library(tidyverse)
theme_set(ggpubr::theme_pubr())

ex_decay_function_data <- function(
  tone_len = 0.05,
  n_gram_length = 2,
  buffer_length_time = 2,
  buffer_length_items = 15,
  buffer_weight = 1,
  stm_duration = 1.5,
  stm_weight = 0.7,
  ltm_half_life = 3.5, 
  ltm_weight = 0.2,
  ltm_asymptote = 0.1,
  x_from = 0,
  x_to = 3,
  x_by = 0.01) {
  
  alphabet_size <- 100000
  
  mod <- ppm::new_ppm_decay(
    alphabet_size = alphabet_size,
    order_bound = 4,
    ltm_weight = ltm_weight,
    ltm_half_life = ltm_half_life,
    ltm_asymptote = ltm_asymptote,
    noise = 0, 
    stm_weight = stm_weight,
    stm_duration = stm_duration,
    buffer_weight = buffer_weight,
    buffer_length_time = buffer_length_time,
    buffer_length_items = buffer_length_items
  )
  
  seq_time <- seq(from = x_from, to = x_to, by = tone_len)
  seq_symb <- seq_along(seq_time)
  stopifnot(length(seq_symb) <= alphabet_size)
  
  ppm::model_seq(mod,
                 seq = seq_symb,
                 time = seq_time, 
                 train = TRUE,
                 predict = FALSE, 
                 zero_indexed = TRUE)
  
  tibble(pos = seq_along(seq_time),
         time = seq_time,
         tone_len = paste(1000 * tone_len, "ms"),
         weight = map2_dbl(pos, time, ~ ppm::get_weight(mod, 
                                                        n_gram = 1,
                                                        pos = .x, 
                                                        time = .y, 
                                                        update_excluded = FALSE,
                                                        zero_indexed = TRUE)))
}

ex_decay_function <- function(
  tone_len = 0.05,
  ltm_weight = 0.2,
  buffer_weight = 1,
  buffer_length_time = 2,
  buffer_length_items = 15,
  stm_weight = 0.7,
  stm_duration = 1.5,
  x_from = 0,
  x_to = 5,
  ltm_asymptote = 0.1,
  buffer_h_line = TRUE,
  stm_h_line = TRUE,
  ltm_h_line = TRUE,
  asymptote_h_line = TRUE,
  legend_position = "right",
  x_axis = "time",
  sec_axis = FALSE,
  ...
) {
  df <- map(tone_len, 
            ex_decay_function_data,
            ltm_weight = ltm_weight,
            buffer_weight = buffer_weight,
            buffer_length_time = buffer_length_time,
            buffer_length_items = buffer_length_items,
            stm_weight = stm_weight,
            stm_duration = stm_duration,
            ltm_asymptote = ltm_asymptote,
            x_from = x_from,
            x_to = x_to, 
            ...) %>% 
    bind_rows()
  
  p <- df %>% 
    ggplot(aes_string(x_axis, "weight")) +
    coord_cartesian(xlim = c(x_from, x_to),
                    # ylim = c(- 0.1, max(df$weight * 1.1)),
                    clip = "off") +
    
    scale_x_continuous("Time (s)", sec.axis = if (sec_axis) sec_axis(~ . / tone_len, 
                                                                     "Symbol number") else
                                                                       waiver()) +
    scale_y_continuous("Weight", limits = c(0, NA)) + 
    theme(aspect.ratio = 1, 
          plot.margin = unit(c(0.5, 1.25, 0.5, 0.5), "cm"), 
          legend.position = legend_position)
  
  x_max <- df[[x_axis]] %>% max()
  
  if (ltm_h_line) p <- p + 
    geom_hline(yintercept = ltm_weight, colour = "darkblue", linetype = "dotted") +
    annotate("text", x = x_max * 1.1, y = ltm_weight, label = "italic(w)[2]", parse = TRUE)
  
  if (asymptote_h_line) p <- p + 
    geom_hline(yintercept = ltm_asymptote, colour = "darkblue", linetype = "dotted") +
    annotate("text", x = x_max * 1.1, y = ltm_asymptote, label = "italic(w)[infinity]", parse = TRUE)
  
  if (buffer_h_line) p <- p + 
    geom_hline(yintercept = buffer_weight, colour = "darkblue", linetype = "dotted") +
    annotate("text", x = x_max * 1.1, y = buffer_weight, label = "italic(w)[0]", parse = TRUE)
  
  if (stm_h_line) p <- p + 
    geom_hline(yintercept = stm_weight, colour = "darkblue", linetype = "dotted") +
    annotate("text", x = x_max * 1.1, y = stm_weight, label = "italic(w)[1]", parse = TRUE)
  
  
  palette <- viridis::viridis(n = 3, option = "C")
  
  shade_region <- function(col, xmin, xmax) {
    annotate("rect",
             xmin = xmin, xmax = xmax, 
             ymin = -Inf, ymax = Inf,
             fill = col,
             alpha = 0.25)
  }
  
  p <- p + 
    shade_region(palette[3],
                 0, 
                 buffer_length_items * tone_len) +
    shade_region(palette[2], 
                 buffer_length_items * tone_len, 
                 buffer_length_items * tone_len + stm_duration) +
    shade_region(palette[1], 
                 buffer_length_items * tone_len + stm_duration,
                 Inf)
 
  if (length(tone_len) > 1)
    p <- p + scale_colour_viridis_d("Tone duration")
  
  p <- p + {
    if (length(tone_len) == 1) 
      geom_line() else {
        geom_line(aes(colour = tone_len))
      }
  }
  
  p
}

if (FALSE) {
  ex_decay_function(x_to = 7,
                    sec_axis = TRUE)
  ggsave("man/figures/example-decay-kernel.png", width = 4, height = 4, dpi = 300)
  # ggsave("output/example-simple-decay-function.png", width = 3.5, height = 3.5, dpi = 300)
  # ggsave("output/example-simple-decay-function.pdf", width = 3.5, height = 3.5, dpi = 300)
}
