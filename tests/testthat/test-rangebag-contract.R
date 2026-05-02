test_that("Rangebag backend fits and predicts through the registry", {
  if (!requireNamespace("terra", quietly = TRUE)) {
    message("Skipping Rangebag backend test because terra is not installed")
  } else {
    set.seed(42)
    r1 <- terra::rast(nrows = 20, ncols = 20, xmin = 140, xmax = 142, ymin = -24, ymax = -22)
    r2 <- r1
    terra::values(r1) <- seq_len(terra::ncell(r1)) / terra::ncell(r1)
    terra::values(r2) <- rep(seq(0, 1, length.out = 20), each = 20)
    r1[1] <- NA
    r2[1] <- NA
    env <- c(r1, r2)
    names(env) <- c("bio1", "bio12")

    occ <- data.frame(
      species = "Synthetic species",
      longitude = seq(140.15, 141.85, length.out = 24),
      latitude = seq(-23.85, -22.15, length.out = 24),
      source = rep(c("A", "B"), each = 12),
      stringsAsFactors = FALSE
    )

    fit <- fit_sdm_model("rangebag", occ, env, background_n = 120, include_quadratic = FALSE, cv_folds = 2, seed = 99, n_cores = 1, n_bags = 20)
    expect_equal(fit$model_id, "rangebag")
    expect_equal(fit$model_label, "Rangebagging")
    expect_true(is.list(fit$model))
    expect_true(length(fit$model$bags) > 0)
    expect_true(is.data.frame(fit$coefficients))
    expect_true(is.list(fit$cv))
    expect_equal(nrow(fit$occurrence_used), 24)
    expect_true(nrow(fit$background_xy) >= 100)

    output_tif <- tempfile(fileext = ".tif")
    suit <- predict_sdm_model(fit, env, output_tif, n_cores = 1)
    expect_true(inherits(suit, "SpatRaster"))
    expect_equal(names(suit), "suitability")
    expect_true(file.exists(output_tif))
    expect_true(is.na(terra::values(suit)[1, 1]))

    expect_error(predict_rangebag_suitability(fit$model, env, tempfile(fileext = ".tif")), "fit")
  }
})
