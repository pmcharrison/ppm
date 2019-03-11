library(ppm)
library(tidyverse)

x <- new(ppm_simple,
         alphabet_size = 200,
         order_bound = 10, 
         shortest_deterministic = FALSE,
         exclusion = TRUE,
         update_exclusion = FALSE,
         escape = "c")

abra <- as.integer(factor(c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a") )) - 1L

y <- x$model_seq(sample(0:19, size = 10000, replace = TRUE),
                 time = numeric(10000),
                 train = TRUE, 
                 predict = TRUE, 
                 keep_distribution = FALSE, 
                 keep_entropy = FALSE)
y$distribution





x <- new(ppm_simple, 
         alphabet_size = 10,
         order_bound = 10, 
         shortest_deterministic = TRUE,
         exclusion = TRUE,
         update_exclusion = TRUE,
         escape = "a")

y <- x$model_seq(sample(0:9, size = 30, replace = TRUE), 
                 time = numeric(),
                 train = TRUE, 
                 predict = TRUE, 
                 keep_distribution = TRUE, 
                 keep_entropy = TRUE)
View(x$as_tibble())
View(y$as_tibble())


df <- x$as_tibble()
df$n_gram <- paste(lapply(df$n_gram, function(x) paste(x, collapse = " ")))
df
y$as_tibble()

library(ppm)
x <- new(ppm_decay,
         alphabet_size = 3,
         order_bound = 10,
         decay_par = list(
           buffer_length_items = 10,
           buffer_weight = 0.75,
           stm_half_life = 1,
           stm_weight = 0.25,
           ltm_weight = 0,
           noise = 0.5
         ))
y <- x$model_seq(sample(0:2, size = 20, replace = TRUE), 
                 time = 1:20 + 0.5,
                 train = TRUE, 
                 predict = TRUE, 
                 keep_distribution = TRUE, 
                 keep_entropy = TRUE)
x$as_list()
View(x$as_tibble())

View(y$as_tibble())

z <- tst::new_tree(order_bound = 10)
tst::add_seq(z, sample(10, size = 10000, replace = TRUE))


y <- x$model_seq(0:5, 
                 train = FALSE, 
                 predict = TRUE, 
                 keep_distribution = FALSE, 
                 keep_entropy = FALSE)
y$as_tibble()

class(y$as_list()$symbol)
y$as_tibble()

y$as_tibble()

tibble::as_tibble(y$as_data_frame())

new(ppm:::symbol_prediction, rnorm(5))$distribution
