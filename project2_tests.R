#project2_tests

library(pacman)
p_load(testthat)
test_that("Testing Problem 6", {
  expect_equal(happy_df$region[3], "Western Europe")
  expect_equal(happy_df$country[7], "Netherlands")
})

test_that("Testing Problem 7", {
  expect_equal(top_ten_df$region[5], "North America")
  expect_equal(top_ten_df$country[5], "Canada")
  expect_equal(nrow(top_ten_df), 10)
})

test_that("Testing Problem 8", {
  expect_equal(no_freedom_df$region[4], "Western Europe")
  expect_equal(no_freedom_df$country[6], "Sudan")
})
test_that("Testing Problem 9", {
  expect_equal(best_freedom_df$region[5], "Central and Eastern Europe")
  expect_equal(best_freedom_df$country[6], "Australia")
})

test_that("Testing Problem 10", {
  expect_equal(data_2015$gff_stat[6], 2.19346, tolerance = 1e-4)
  expect_equal(data_2015$country[6], "Finland")
})


test_that("Baseball Test Problem 14", {
  expect_equal(nrow(baseball), 726)
})

test_that("Baseball testing number of columns", {
  expect_equal(length(baseball), 18)
})

test_that("Testing Problem 15", {
  expect_equal(baseball$BA[8], .219,, tolerance = 1e-3)
  expect_equal(max(baseball$BA), 1)
  expect_equal(min(baseball$BA), 0)
})

test_that("Testing Problem 16", {
  expect_equal(baseball$OBP[25], .363, tolerance = 1e-3)
  expect_equal(max(baseball$OBP), 1)
  expect_equal(min(baseball$OBP), 0)
})

test_that("Testing Problem 17", {
  expect_equal(strikeout_artist$Last[1], "Incaviglia")
  expect_equal(strikeout_artist$Last[10], "Strawberry")
})

test_that("Testing Problem 18", {
  expect_equal(nrow(eligible_df), 251)
  expect_equal(length(eligible_df), 18)
})






