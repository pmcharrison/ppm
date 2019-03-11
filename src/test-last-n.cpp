#include <testthat.h>

typedef std::vector<int> sequence;
sequence last_n (sequence x, int n);

context("last_n") {

  test_that("example 1") {
    sequence input;
    input.push_back(1);
    input.push_back(2);
    input.push_back(3);

    sequence output = last_n(input, 0);
    expect_true(output.size() == 0);

    output = last_n(input, 1);
    expect_true(output.size() == 1);
    expect_true(output[0] == 3);

    output = last_n(input, 2);
    expect_true(output.size() == 2);
    expect_true(output[0] == 2);
    expect_true(output[1] == 3);
  }

  test_that("example 2") {
    sequence input2;
    sequence output2 = last_n(input2, 0);
    expect_true(output2.size() == 0);
  }

}
