test_that("volume accepts integer dbh and h_top", {
  # Regression: previously class(dbh)!="numeric" rejected integers.
  expect_equal(
    volume(dbh = 20L, h_top = 30L, sp = "spruce"),
    volume(dbh = 20, h_top = 30, sp = "spruce")
  )
})

test_that("volume clamps only the trees where h_vol_lower > h_vol_upper", {
  # Regression: the clamp condition was inverted, which zeroed the *valid*
  # tree and produced a negative volume for the invalid one.
  expect_warning(
    res <- volume(
      dbh = c(30, 30), h_top = c(25, 25),
      h_vol_lower = c(2, 20), h_vol_upper = c(20, 2), sp = "spruce"
    ),
    "h_vol_lower must not be larger than h_vol_upper"
  )

  valid <- volume(dbh = 30, h_top = 25, h_vol_lower = 2, h_vol_upper = 20, sp = "spruce")

  expect_equal(res[1], valid)  # valid tree untouched
  expect_equal(res[2], 0)      # invalid tree clamped to a zero-width interval
  expect_true(all(res >= 0))   # never negative
})

test_that("volume default single- and multi-tree results are unchanged", {
  expect_equal(volume(30, 25, sp = "spruce"), 0.8100256, tolerance = 1e-6)
  expect_equal(
    volume(dbh = c(20, 25, 30), h_top = c(30, 25, 37)),
    c(0.4861970, 0.5913435, 1.2419804),
    tolerance = 1e-6
  )
})
