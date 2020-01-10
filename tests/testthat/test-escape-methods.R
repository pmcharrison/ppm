context("test-escape-methods")

load <- function(file) {
  env <- new.env()
  source(file, local = env)
  env$x[[1]]
}

test_that("A", {
  mod <- ppm::new_ppm_simple(5, escape = "a", update_exclusion = FALSE, shortest_deterministic = TRUE, order_bound = 100)
  df <- ppm::model_seq(mod, factor(c("a", "b", "r", "a", 'c', 'a', 'd', 'a', 'b', 'r', 'a')))
  expect_equal(
    df$distribution,
    load("data/escape-a.R"),
    tolerance = 1e-5, check.names = FALSE)
})

test_that("B", {
  mod <- ppm::new_ppm_simple(5, escape = "b", update_exclusion = FALSE, shortest_deterministic = TRUE, order_bound = 100)
  df <- ppm::model_seq(mod, factor(c("a", "b", "r", "a", 'c', 'a', 'd', 'a', 'b', 'r', 'a')))
  # df <- ppm::model_seq(mod, factor(c("a", "b", "r", "a", "c"), levels = c("a", "b", "c", "d", "r")))
  ref <- load("data/escape-b.R")
  expect_equal(
    df$distribution,
    ref,
    tolerance = 1e-5, check.names = FALSE)
})

test_that("C", {
  mod <- ppm::new_ppm_simple(5, escape = "c", update_exclusion = FALSE, shortest_deterministic = TRUE, order_bound = 100)
  df <- ppm::model_seq(mod, factor(c("a", "b", "r", "a", 'c', 'a', 'd', 'a', 'b', 'r', 'a')))
  expect_equal(
    df$distribution,
    load("data/escape-c.R"),
    tolerance = 1e-5, check.names = FALSE)
})

test_that("D", {
  mod <- ppm::new_ppm_simple(5, escape = "d", update_exclusion = FALSE, shortest_deterministic = TRUE, order_bound = 100)
  df <- ppm::model_seq(mod, factor(c("a", "b", "r", "a", 'c', 'a', 'd', 'a', 'b', 'r', 'a')))
  expect_equal(
    df$distribution,
    load("data/escape-d.R"),
    tolerance = 1e-5, check.names = FALSE)
})

test_that("AX", {
  mod <- ppm::new_ppm_simple(5, escape = "ax", update_exclusion = FALSE, shortest_deterministic = TRUE, order_bound = 100)
  df <- ppm::model_seq(mod, factor(c("a", "b", "r", "a", 'c', 'a', 'd', 'a', 'b', 'r', 'a')))
  expect_equal(
    df$distribution,
    load("data/escape-ax.R"),
    tolerance = 1e-5, check.names = FALSE)
})

test_that("AX (v2)", {
  mod <- ppm::new_ppm_simple(5, escape = "ax", update_exclusion = FALSE, shortest_deterministic = TRUE, order_bound = 100)
  df <- ppm::model_seq(mod, factor(c("a", "b", "r", "a", 'c', 'a', 'd', 'a', 'b', 'r', 'a')))
  expect_equal(
    df$distribution,
    list(
      c(0.19999999, 0.19999999, 0.19999999, 0.19999999, 0.19999999),
      c(0.46666667, 0.13333333, 0.13333333, 0.13333333, 0.13333333),
      c(0.30434784, 0.30434784, 0.1304348, 0.1304348, 0.1304348),
      c(0.2413793, 0.2413793, 0.13793103, 0.13793103, 0.2413793),
      c(0.24999999, 0.4166667, 0.08333333, 0.08333333, 0.16666666),
      c(0.26666665, 0.19999997, 0.19999997, 0.13333331, 0.19999997),
      c(0.234375, 0.265625, 0.265625, 0.09375, 0.140625),
      c(0.25, 0.1875, 0.1875, 0.1875, 0.1875),
      c(0.22222222, 0.20987655, 0.20987655, 0.20987655, 0.14814815),
      c(0.22857143, 0.17142858, 0.14285713, 0.14285713, 0.31428573),
      c(0.38983053, 0.16949153, 0.13559322, 0.13559322, 0.16949153)
    ),
    tolerance = 1e-5)
})
