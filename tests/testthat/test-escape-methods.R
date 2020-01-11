context("test-escape-methods")

load <- function(file) {
  env <- new.env()
  source(file, local = env)
  env$x[[1]]
}

test <- function(escape, update_exclusion, file) {
  mod <- ppm::new_ppm_simple(5, escape = escape, update_exclusion = update_exclusion, 
                             shortest_deterministic = TRUE, order_bound = 100)
  df <- ppm::model_seq(mod, factor(c("a", "b", "r", "a", 'c', 'a', 'd', 'a', 'b', 'r', 'a')))
  expect_equal(
    df$distribution,
    load(file.path(getwd(), "data", file)),
    tolerance = 1e-5, check.names = FALSE)
}

test_that("different escape methods, without update exclusion", {
  test("a", update_exclusion = FALSE, "escape-a.R")
  test("b", update_exclusion = FALSE, "escape-b.R")
  test("c", update_exclusion = FALSE, "escape-c.R")
  test("d", update_exclusion = FALSE, "escape-d.R")
  test("ax", update_exclusion = FALSE, "escape-ax.R")
})

test_that("different escape methods, without update exclusion", {
  test("a", update_exclusion = TRUE, "escape-a-update-excluded.R")
  test("b", update_exclusion = TRUE, "escape-b-update-excluded.R")
  test("c", update_exclusion = TRUE, "escape-c-update-excluded.R")
  test("d", update_exclusion = TRUE, "escape-d-update-excluded.R")
  test("ax", update_exclusion = TRUE, "escape-ax-update-excluded.R")
})
