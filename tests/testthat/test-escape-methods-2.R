context("test-escape-methods-2")

load <- function(file) {
  env <- new.env()
  source(file, local = env)
  env$x
}

test <- function(escape, update_exclusion, file) {
  levels <- c('a', 'b', 'c', 'd', 'e', 'i', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'w')
  seqs <- list(
    c('a', 'b', 'r', 'a', 'c', 'a', 'd', 'a', 'b', 'r', 'a'),
    c('l', 'e', 't', 'l', 'e', 't', 't', 'e', 'r', 't', 'e', 'l', 'e'),
    c('a', 's', 's', 'a', 'n', 'i', 's', 's', 'i', 'm', 'a', 's', 's', 'a'),
    c('m', 'i', 's', 's', 'i', 's', 's', 'i', 'p', 'p', 'i'),
    c('w', 'o', 'o', 'l', 'o', 'o', 'b', 'o', 'o', 'l', 'o', 'o')
  ) %>% 
    lapply(factor, levels = levels)
  
  mod <- ppm::new_ppm_simple(length(levels), escape = escape, update_exclusion = update_exclusion, 
                             shortest_deterministic = TRUE, order_bound = 100)
  res <- lapply(seqs, function(s) model_seq(mod, s))
  
  expect_equal(
    lapply(res, function(x) x$distribution),
    load(file.path(getwd(), "data", file)),
    tolerance = 1e-5, check.names = FALSE
  )
}

# These regression tests come from IDyOM v 1.5 (Pearce, 2005)
test_that("different escape methods, without update exclusion", {
  test("a", update_exclusion = FALSE, "escape-v2-a.R")
  test("b", update_exclusion = FALSE, "escape-v2-b.R")
  test("c", update_exclusion = FALSE, "escape-v2-c.R")
  test("d", update_exclusion = FALSE, "escape-v2-d.R")
  test("ax", update_exclusion = FALSE, "escape-v2-ax.R")
})

# Note! The LISP implementation of Pearce (2005) has mistakes
# in the implementation of update exclusion.
# These regression tests come from the latest version of mtp_development
# (as of Jan 2020) which has fixed these problems.
test_that("different escape methods, without update exclusion", {
  test("a", update_exclusion = TRUE, "escape-v2-a-update-excluded.R")
  test("b", update_exclusion = TRUE, "escape-v2-b-update-excluded.R")
  test("c", update_exclusion = TRUE, "escape-v2-c-update-excluded.R")
  test("d", update_exclusion = TRUE, "escape-v2-d-update-excluded.R")
  test("ax", update_exclusion = TRUE, "escape-v2-ax-update-excluded.R")
})
