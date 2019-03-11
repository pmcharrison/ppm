#include <testthat.h>

std::vector<double> normalise_distribution(std::vector<double>);

context("Normalise distribution") {

  test_that("example 1") {
    std::vector<double> input;
    input.push_back(2);
    input.push_back(1);
    input.push_back(1);
    
    std::vector<double> output = normalise_distribution(input);
    
    expect_true(output[0] == 0.5);
    expect_true(output[1] == 0.25);
    expect_true(output[2] == 0.25);
  }
  
  test_that("example 2") {
    std::vector<double> input;
    std::vector<double> output = normalise_distribution(input);
    
    expect_true(output.size() == 0);
  }

}
