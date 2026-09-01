library(testthat)

test_that("CES handles conjunctive and disjunctive structures", {
  
  pa <- .1
  pb <- .9
  
  conjModel <- list(
    e = "a&b",
    a = pa,
    b = pb
  )
  
  disjModel <- list(
    e = "a|b",
    a = pa,
    b = pb
  )
  
  actual_world <- list(
    e = 1,
    a = 1,
    b = 1
  )
  
  # Conjunctive structure
  expect_equal(
    compute_judgment(
      "a", "e", conjModel, actual_world, "ces", .7
    ),
    0.9472197,
    tolerance = 1e-7
  )
  
  expect_equal(
    compute_judgment(
      "b", "e", conjModel, actual_world, "ces", .7
    ),
    0.2739082,
    tolerance = 1e-7
  )
  
  # Disjunctive structure
  expect_equal(
    compute_judgment(
      "a", "e", disjModel, actual_world, "ces", .7
    ),
    0.1485895,
    tolerance = 1e-7
  )
  
  expect_equal(
    compute_judgment(
      "b", "e", disjModel, actual_world, "ces", .7
    ),
    0.513847,
    tolerance = 1e-7
  )
})
