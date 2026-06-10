test_that("plot_taper runs without error for single, multi, and bark-vector cases", {
  pdf(NULL)  # null graphics device
  on.exit(dev.off(), add = TRUE)

  expect_no_error(plot_taper(33, 30, sp = "spruce"))
  expect_no_error(plot_taper(dbh = c(33, 20, 18), h_top = c(30, 25, 20), sp = c(1, 1, 3)))
  expect_no_error(plot_taper(dbh = rep(25, 2), h_top = rep(27, 2), sp = 1,
                             with_bark = c(TRUE, FALSE)))
})

test_that("plot_taper single-tree label keeps the species name", {
  # Regression: with_bark recycling overwrote `sp`, so the label printed
  # "species= TRUE" instead of the species name. The label is only observable
  # via the mtext() call, so capture its first argument. The capture target
  # lives in the global environment so trace()'s tracer (evaluated in the
  # mtext call frame) can find it.
  assign(".taper_test_label", NA_character_, envir = globalenv())
  on.exit(suppressWarnings(rm(".taper_test_label", envir = globalenv())), add = TRUE)

  trace(graphics::mtext,
        tracer = quote(assign(".taper_test_label", text, envir = globalenv())),
        print = FALSE)
  on.exit(untrace(graphics::mtext), add = TRUE)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  plot_taper(33, 30, sp = "spruce")

  expect_match(get(".taper_test_label", envir = globalenv()), "species= spruce")
})
