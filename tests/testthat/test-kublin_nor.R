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
  Dx_vals <- kublin_nor(Hx = c(1.3, 5, 10), Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = 1)

  expect_length(Dx_vals, 3)
  expect_type(Dx_vals, "double")
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
