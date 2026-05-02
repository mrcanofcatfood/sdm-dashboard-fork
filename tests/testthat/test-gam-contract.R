test_that("GAM backend fits and predicts through the registry", {
  if (!requireNamespace("terra", quietly = TRUE) || !requireNamespace("mgcv", quietly = TRUE)) {
    message("Skipping GAM backend test because terra or mgcv is not installed")
  } else {
    set.seed(42)
    r1 <- terra::rast(nrows = 20, ncols = 20, xmin = 140, xmax = 142, ymin = -24, ymax = -22)
    r2 <- r1
    terra::values(r1) <- seq_len(terra::ncell(r1)) / terra::ncell(r1)
    terra::values(r2) <- rep(seq(0, 1, length.out = 20), each = 20)
    env <- c(r1, r2)
    names(env) <- c("bio1", "bio12")

    occ <- data.frame(
      species = "Synthetic species",
      longitude = seq(140.15, 141.85, length.out = 24),
      latitude = seq(-23.85, -22.15, length.out = 24),
      source = rep(c("A", "B"), each = 12),
      stringsAsFactors = FALSE
    )

    fit <- fit_sdm_model("gam", occ, env, background_n = 120, include_quadratic = FALSE, cv_folds = 2, seed = 99, n_cores = 1)
    expect_equal(fit$model_id, "gam")
    expect_equal(fit$model_label, "GAM / Smooth response curves")
    expect_true(inherits(fit$model, "gam"))
    expect_true(inherits(fit$formula, "formula"))
    expect_true(is.data.frame(fit$coefficients))
    expect_true(is.list(fit$cv))
    expect_equal(nrow(fit$occurrence_used), 24)
    expect_true(nrow(fit$background_xy) >= 100)

    output_tif <- tempfile(fileext = ".tif")
    suit <- predict_sdm_model(fit, env, output_tif, n_cores = 1)
    expect_true(inherits(suit, "SpatRaster"))
    expect_equal(names(suit), "suitability")
    expect_true(file.exists(output_tif))
  }
})

test_that("GAM formula uses linear terms for low-cardinality covariates", {
  d <- data.frame(presence = c(0, 1, 0, 1, 0, 1), low = c(0, 0, 1, 1, 1, 0), high = seq(0, 1, length.out = 6))
  formula_text <- paste(deparse(make_gam_formula(c("low", "high"), d)), collapse = " ")
  expect_true(grepl("low", formula_text, fixed = TRUE))
  expect_false(grepl("s(low", formula_text, fixed = TRUE))
  expect_true(grepl("s(high", formula_text, fixed = TRUE))
})
