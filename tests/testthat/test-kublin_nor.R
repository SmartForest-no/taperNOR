test_that("kublin_nor returns a single height for a single Dx", {
  Hx <- kublin_nor(Dx = 25, Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = 1)

  expect_length(Hx, 1)
  expect_type(Hx, "double")
})

test_that("kublin_nor returns one height per element for multi-element Dx", {
  Dx <- c(25, 15)

  Hx <- kublin_nor(Dx = Dx, Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = 1)

  expect_length(Hx, length(Dx))
  expect_type(Hx, "double")

  # Looping manually over Dx should give the same result as passing the
  # vector directly.
  Hx_loop <- sapply(Dx, function(d) {
    kublin_nor(Dx = d, Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = 1)
  })

  expect_equal(Hx, unname(Hx_loop))
})

test_that("kublin_nor still works for Hx input", {
  # The Hx branch returns the full TapeR::E_DHx_HmDm_HT.f() list; the
  # predicted diameters are in $DHx, one per element of Hx.
  res <- kublin_nor(Hx = c(1.3, 5, 10), Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = 1)

  expect_type(res, "list")
  expect_length(res$DHx, 3)
  expect_type(res$DHx, "double")
})

test_that("kublin_nor errors when neither or both of Hx and Dx are given", {
  expect_error(
    kublin_nor(Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = 1),
    "Either Hx or Dx must be given"
  )

  expect_error(
    kublin_nor(Hx = 5, Dx = 25, Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = 1),
    "Either Hx or Dx must be given"
  )
})

test_that("kublin_nor accepts spruce via its aliases", {
  # Numeric 1 and the string aliases should all resolve to the spruce model.
  ref <- kublin_nor(Dx = 25, Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = 1)

  for (alias in c("spruce", "s", "gran", "g", "1")) {
    expect_equal(
      kublin_nor(Dx = 25, Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = alias),
      ref
    )
  }
})

test_that("kublin_nor errors for pine and birch (only spruce is fitted)", {
  # Regression: pine/birch previously fell through to the spruce model
  # silently. They must now error rather than return spruce-based results.
  # "bjork" (ASCII) covers the birch path; the non-ASCII Norwegian alias is
  # exercised by the function but kept out of this test to keep the source ASCII.
  for (alias in c("pine", "p", "furu", "f", "2",
                  "birch", "b", "bjork", "bj", "lauv", "l", "3")) {
    expect_error(
      kublin_nor(Dx = 25, Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = alias),
      "only supports spruce"
    )
  }
})

test_that("kublin_nor errors for an unrecognized species", {
  expect_error(
    kublin_nor(Dx = 25, Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = "oak"),
    "sp must indicate spruce"
  )
})
