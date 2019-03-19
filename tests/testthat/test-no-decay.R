context("no-decay")

library(magrittr)

start <- Sys.time()

test_ppm <- function(seqs, answer, tolerance = 1e-4, alphabet = NULL, ...) {
  stopifnot(is.list(seqs), is.list(answer))
  if (is.null(alphabet)) alphabet <- do.call(c, seqs) %>% unique %>% sort
  model <- new_ppm_simple(alphabet_size = length(alphabet), ...)
  res <- list()
  for (i in seq_along(seqs)) {
    seq <- as.integer(factor(seqs[[i]], levels = alphabet)) - 1L
    res[[i]] <- model_seq(model = model, seq = seq, zero_indexed = TRUE)
  }
  for (i in seq_along(res)) {
    expect_equal(nrow(res[[i]]), length(answer[[i]]))
    for (j in seq_len(nrow(res[[i]]))) {
      x <- res[[i]]$distribution[[j]] %>% as.numeric
      y <- answer[[i]][[j]] %>% as.numeric
      test <- isTRUE(all.equal(x, y, tolerance = tolerance))
      if (!test) browser()
      expect_true(test)
    }
  }
}

test_ppm2 <- function(seqs, answer, tolerance = 1e-4, alphabet = NULL, ...) {
  stopifnot(is.list(seqs), is.numeric(answer))
  if (is.null(alphabet)) alphabet <- do.call(c, seqs) %>% unique %>% sort
  model <- new_ppm_simple(alphabet_size = length(alphabet), ...)
  res <- rep(as.numeric(NA), times = length(seqs))
  for (i in seq_along(seqs)) {
    seq <- as.integer(factor(seqs[[i]], levels = alphabet)) - 1L
    tmp <- model_seq(model = model, seq = seq, zero_indexed = TRUE)
    res[i] <- mean(tmp$information_content)
  }
  expect_equal(res, answer, tolerance = tolerance)
}

test_that("ppm*-dataset1", {
  abra <- c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a")
  test_ppm(seqs = list(abra, abra), tolerance = 1e-3,
           update_exclusion = FALSE,
           answer = list(
             list(
               c(0.2, 0.2, 0.2, 0.2, 0.2),
               c(0.6, 0.1, 0.1, 0.1, 0.1),
               c(0.3333, 0.3333, 0.1111, 0.1111, 0.1111),
               c(0.25, 0.25, 0.125, 0.125, 0.25),
               c(0.2000, 0.5333, 0.0667, 0.0667, 0.1333),
               c(0.2667, 0.2, 0.2, 0.1333, 0.2),
               c(0.2083, 0.2917, 0.2917, 0.0833, 0.1250),
               c(0.2500, 0.1875, 0.1875, 0.1875, 0.1875),
               c(0.2093, 0.2171, 0.2171, 0.2171, 0.1395),
               c(0.1915, 0.1489, 0.1277, 0.1277, 0.4043),
               c(0.4348, 0.1522, 0.1304, 0.1304, 0.1522)
             ),
             list(
               c(.2777778, .19444445, .16666667, .16666667, .19444445),
               c(.20245397, .28834355, .19018403, .19018403, .12883434),
               c(.15714285, .11428571, .08571428, .08571428, .55714285),
               c(.5692308, .12307692, .09230768, .09230768, .12307692),
               c(.11999999, .23, .46, .10999999, .07999999),
               c(.47272727, .14545456, .12727273, .10909091, .14545456),
               c(.111428574, .20571429, .15142857, .46285713, .068571426),
               c(.48275864, .13793103, .12068966, .12068966, .13793103),
               c(.11351351, .5243243, .14864865, .14864865, .06486486),
               c(.12612611, .08108108, .06306306, .06306306, .6666666),
               c(.6701031, .09278351, .072164945, .072164945, .09278351)
             )))
})

test_that("IDyOM ppm*-dataset1+2", {
  abra <- c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a")
  test_ppm(seqs = list(abra, abra, abra),
           update_exclusion = FALSE,
           answer = list(
             list(
               c(.19999999, .19999999, .19999999, .19999999, .19999999),
               c(.59999996, .09999999, .09999999, .09999999, .09999999),
               c(.33333334, .33333334, .11111111, .11111111, .11111111),
               c(.25, .25, .125, .125, .25),
               c(.20000002, .53333336, .06666668, .06666668, .13333336),
               c(.26666665, .19999997, .19999997, .13333331, .19999997),
               c(.20833334, .2916667, .2916667, .083333336, .125),
               c(.25, .1875, .1875, .1875, .1875),
               c(.2093023, .21705426, .21705426, .21705426, .13953489),
               c(.19148937, .14893615, .12765956, .12765956, .40425533),
               c(.4347826, .1521739, .13043477, .13043477, .1521739)
             ),
             list(
               c(.2777778, .19444445, .16666667, .16666667, .19444445),
               c(.20245397, .28834355, .19018403, .19018403, .12883434),
               c(.15714285, .11428571, .08571428, .08571428, .55714285),
               c(.5692308, .12307692, .09230768, .09230768, .12307692),
               c(.11999999, .23, .46, .10999999, .07999999),
               c(.47272727, .14545456, .12727273, .10909091, .14545456),
               c(.111428574, .20571429, .15142857, .46285713, .068571426),
               c(.48275864, .13793103, .12068966, .12068966, .13793103),
               c(.11351351, .5243243, .14864865, .14864865, .06486486),
               c(.12612611, .08108108, .06306306, .06306306, .6666666),
               c(.6701031, .09278351, .072164945, .072164945, .09278351)
             ),
             list(
               c(.31914893, .19148934, .14893617, .14893617, .19148934),
               c(.15789472, .35197368, .20065789, .20065789, .088815786),
               c(.107382536, .06711409, .04697986, .04697986, .7315436),
               c(.73015875, .07936508, .055555556, .055555556, .07936508),
               c(.06447536, .17699116, .6384324, .082174465, .037926678),
               c(.61956525, .10869565, .08695652, .07608695, .10869565),
               c(.06033518, .16201115, .103910595, .64022344, .033519547),
               c(.625, .10416666, .08333333, .08333333, .10416666),
               c(.06620208, .67595816, .11149825, .11149825, .034843203),
               c(.090047404, .052132707, .037914697, .037914697, .78199047),
               c(.7790698, .0639535, .046511635, .046511635, .0639535)
             )))
})

test_that("IDyOM PPM* dataset 3", {
  seqs <- c(
    "abracadabra", "letlettertele", "assanissimassa",
    "mississippi", "wooloobooloo"
  ) %>% strsplit(split = "")
  env <- new.env()
  if (interactive())
    source("tests/testthat/data/idyom-3.R", local = env) else
      source("data/idyom-3.R", local = env)
  answer <- env$x
  test_ppm(seqs, answer = answer, update_exclusion = FALSE)
})

test_that("IDyOM update exclusion abracadabra-x2", {
  seqs <- c("abracadabra", "abracadabra") %>%
    strsplit(split = "")
  answer <- c(2.4509745, 1.1457417)
  test_ppm2(seqs, answer, update_exclusion = TRUE)
})

test_that("IDyOM update exclusion abracadabra-x3", {
  seqs <- c("abracadabra", "abracadabra", "abracadabra") %>%
    strsplit(split = "")
  answer <- c(2.4509745, 1.1457417, 0.8534344)
  test_ppm2(seqs, answer, update_exclusion = TRUE)
})

test_that("IDyOM update exclusion abracadabra-bracadabra", {
  seqs <- c("abracadabra", "bracadabra") %>%
    strsplit(split = "")
  answer <- c(2.4509745, 1.1973304)
  test_ppm2(seqs, answer, update_exclusion = TRUE)
})

test_that("IDyOM update exclusion abracadabra-abrabrac", {
  seqs <- c("abracadabra", "abrabrac") %>%
    strsplit(split = "")
  answer <- c(2.4509745, 1.4018788)
  test_ppm2(seqs, answer, update_exclusion = TRUE)
})

test_that("IDyOM update exclusion abracadabra-abratbrac", {
  seqs <- c("abracadabra", "abratbrac") %>%
    strsplit(split = "")
  answer <- c(2.591134, 1.8791169)
  test_ppm2(seqs, answer, update_exclusion = TRUE, order_bound = 30)
})

test_that("IDyOM update exclusion abracadabra-abrabtrac", {
  seqs <- c("abracadabra", "abrabtrac") %>%
    strsplit(split = "")
  answer <- c(2.591134, 2.0285676)
  test_ppm2(seqs, answer, update_exclusion = TRUE)
})

test_that("IDyOM update exclusion abracadabra-abrabrtac", {
  seqs <- c("abracadabra", "abrabrtac") %>%
    strsplit(split = "")
  answer <- c(2.591134, 1.8929263)
  test_ppm2(seqs, answer, update_exclusion = TRUE)
})

test_that("IDyOM update exclusion abracadabra-abrabratc", {
  seqs <- c("abracadabra", "abrabratc") %>%
    strsplit(split = "")
  answer <- c(2.591134, 1.7575804)
  test_ppm2(seqs, answer, update_exclusion = TRUE)
})

test_that("IDyOM update exclusion abracadabra-abrabract", {
  seqs <- c("abracadabra", "abrabract") %>%
    strsplit(split = "")
  answer <- c(2.591134, 1.5876032)
  test_ppm2(seqs, answer, update_exclusion = TRUE)
})

test_that("IDyOM PPM* with mixtures", {
  seqs <- c(
    "abracadabrac", "letlettertele", "assanissimassa",
    "mississippi", "agcgacgag"
  ) %>% strsplit(split = "")
  files <- paste(if (interactive()) "tests/testthat/",
                 "data/", 
                 paste("ppm-mix-", 1:5, ".R", sep = ""),
                 sep = "")
  
  for (i in seq_along(seqs)) {
    seq <- seqs[i]
    file <- files[i]
    env <- new.env()
    source(file, local = env)
    answer <- env$x
    test_ppm(seq, answer = answer, update_exclusion = FALSE)
  }
})

test_that("IDyOM PPM* with mixtures and update exclusion", {
  seqs <- c("abracadabrac") %>% strsplit(split = "")
  files <- paste(if (interactive()) "tests/testthat/",
                 "data/", 
                 paste("ppm-mix-ui-", 1, ".R", sep = ""),
                 sep = "")
  
  for (i in seq_along(seqs)) {
    seq <- seqs[i]
    file <- files[i]
    env <- new.env()
    source(file, local = env)
    answer <- env$x
    test_ppm(seq, answer = answer, update_exclusion = TRUE)
  }
})

test_that("IDyOM PPM with mixtures and order bound = 0, 1", {
  seqs <- c("abracadabrac", "abracadabrac") %>% strsplit(split = "")
  files <- paste(if (interactive()) "tests/testthat/",
                 "data/", 
                 paste("ppm-fix-", 0:1, ".R", sep = ""),
                 sep = "")
  
  for (i in seq_along(seqs)) {
    seq <- seqs[i]
    file <- files[i]
    order_bound <- (0:1)[i]
    env <- new.env()
    source(file, local = env)
    answer <- env$x
    test_ppm(seq, answer = answer, order_bound = order_bound,
             update_exclusion = FALSE)
  }
})

test_that("Louis Couperin: unmeasured prelude, no. 7", {
  seqs <- paste0("45 52 57 60 64 69 45 52 57 60 64 69 45 45 44 52 60 ",
                 "60 59 64 64 71 45 57 76 74 72 71 72 72 72 72 72 72 ",
                 "72 72 52 64 69 69 71 71 71 71 71 71 50 52 54 55 52 ",
                 "53 52 50 48 48 47 69 71 72 55 74 62 67 48 67 69 71 ",
                 "57 72 72 57 64 65 72 72 72 60 62 66 71 71 62 68 66 ",
                 "68 64 64 64 64 59 59 60 64 68 68 69 69 50 49 49 50 ",
                 "59 64 64 65 69 47 48 50 52 53 50 50 52 60 64 69 52 ",
                 "60 60 59 69 69 68 68 68 45 52 57 61 64 69 45 52 57 ",
                 "62 61 64 69") %>%
    strsplit(split = " ") %>% magrittr::extract2(1) %>% list
  answer <- c(4.195832)
  test_ppm2(seqs, answer, update_exclusion = FALSE)
})

a <- Sys.time()
test_that("Paul Hindemith: Acht Stücke Für Flöte Allein: VI. Lied, Leicht Bewegt", {
  seqs <- "84 83 85 86 81 82 81 80 80 80 79 77 79 76 83 73 72 71 72 77 76 75 77 79 80 82
    84 85 87 88 88 87 82 77 78 83 81 79 77 76 75 77 78 73 74 72 71 71 70 69 68 67
    68 70 62 73 74 63 78 79 92 90 89 88 88 91 87 86 85 84 83 85 86 81 82 81 80 80
    80 79 77 79 76 83 73 71 73 74 74 74 73 71 73 70 77 67 68 69 70 70 68 69 70 70
    68 69 70 63 63 68 66 63 62 70 63 68 66 63 62 70 63 68 66 63 62" %>%
    gsub("(\n| )+", " ", .) %>%
    strsplit(split = " ") %>% magrittr::extract2(1) %>% list
  answer <- c(4.210231)
  test_ppm2(seqs, answer, update_exclusion = FALSE)
})
b <- Sys.time()
b - a

time_taken <- Sys.time() - start
