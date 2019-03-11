#include <testthat.h>
#include <cmath>

double compute_entropy (std::vector<double> x);

context("compute_entropy") {

  test_that("example 1") {
    std::vector<double> x1;
    x1.push_back(0.5);
    x1.push_back(0.25);
    x1.push_back(0.25);
    expect_true(round(1000 * compute_entropy(x1)) / 1000 == 1.5);
    
    std::vector<double> x2;
    x2.push_back(0.1);
    x2.push_back(0.2);
    x2.push_back(0.7);
    expect_true(round(1000 * compute_entropy(x2)) / 1000 == 1.157);
    
    
    std::vector<double> x3;
    x3.push_back(0.01);
    x3.push_back(0.09);
    x3.push_back(0.9);
    expect_true(round(1000 * compute_entropy(x3)) / 1000 == 0.516);
    
  }
}
