# Focused unit and edge-case test suite for Q2(a) data cleaning and cost matrix generation.
#
# Usage from repository root:
#   Rscript code/q2/test_q2a_cost_matrix.R

locate_repo_root <- function() {
  file_argument <- grep("^--file=", commandArgs(), value = TRUE)
  starting_points <- getwd()

  if (length(file_argument) > 0L) {
    script_path <- sub("^--file=", "", file_argument[[1L]])
    starting_points <- c(dirname(script_path), starting_points)
  }

  for (start in starting_points) {
    candidate <- normalizePath(start, winslash = "/", mustWork = TRUE)
    repeat {
      if (file.exists(file.path(candidate, "CPC351-Assignment-1.Rproj")) &&
          file.exists(file.path(candidate, "code", "run.R"))) {
        return(candidate)
      }
      parent <- dirname(candidate)
      if (identical(parent, candidate)) break
      candidate <- parent
    }
  }

  stop("Cannot find repository root.", call. = FALSE)
}

assert_raises_error <- function(expr, description, expected_message = NULL) {
  caught <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
  if (is.null(caught)) {
    stop("Expected error did not occur for: ", description, call. = FALSE)
  }
  if (!is.null(expected_message) &&
      !grepl(expected_message, conditionMessage(caught), fixed = TRUE)) {
    stop("Unexpected error for ", description, ": ", conditionMessage(caught),
         call. = FALSE)
  }
}

make_temp_csv <- function(df) {
  tf <- tempfile(fileext = ".csv")
  utils::write.csv(df, tf, row.names = FALSE, na = "NA")
  tf
}

run_q2a_tests <- function(project_root = locate_repo_root()) {
  source(file.path(project_root, "code", "q2", "q2a_cost_matrix.R"), local = TRUE)

  cat("=== Running Q2(a) Cleaning & Cost Matrix Test Suite ===\n")

  pts <- q2_delivery_points()
  p1_x <- pts$x[[1L]]; p1_y <- pts$y[[1L]] # P1 (60, 200)
  p2_x <- pts$x[[2L]]; p2_y <- pts$y[[2L]] # P2 (180, 200)
  p3_x <- pts$x[[3L]]; p3_y <- pts$y[[3L]] # P3 (80, 180)
  p4_x <- pts$x[[4L]]; p4_y <- pts$y[[4L]] # P4 (140, 180)

  # Helper to make a standard row
  make_row <- function(ox, oy, dx, dy, weather = "Rainy", time = "Morning", tt = 15) {
    data.frame(
      Origin_X = ox, Origin_Y = oy, Dest_X = dx, Dest_Y = dy,
      Weather = weather, Time_of_Day = time, Travel_Time_Minutes = tt,
      stringsAsFactors = FALSE
    )
  }

  # 1. Known type-7 75th percentile: c(10, 20, 30, 40) gives 32.5
  df_q7 <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 10),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 20),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 30),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 40)
  )
  tf_q7 <- make_temp_csv(df_q7); on.exit(unlink(tf_q7), add = TRUE)
  res_q7 <- build_q2a_cost_matrix(tf_q7)
  stopifnot(
    identical(unname(res_q7$cost_matrix["P1", "P2"]), 32.5),
    identical(res_q7$route_status_matrix["P1", "P2"], "usable_history"),
    isFALSE(res_q7$penalty_matrix["P1", "P2"])
  )
  cat("[PASS] 1. Known Type-7 75th percentile calculation (c(10,20,30,40) == 32.5)\n")

  # 2. Mixed valid times and closures (NA and -1 are excluded before 75th percentile)
  df_mixed <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 10),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = -1),  # closure
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 20),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = NA),  # closure
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 30),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 40)
  )
  tf_mixed <- make_temp_csv(df_mixed); on.exit(unlink(tf_mixed), add = TRUE)
  res_mixed <- build_q2a_cost_matrix(tf_mixed)
  stopifnot(
    identical(unname(res_mixed$cost_matrix["P1", "P2"]), 32.5),
    identical(res_mixed$stats$missing_travel_times, 1L),
    identical(res_mixed$stats$closure_records_minus_one, 1L),
    identical(res_mixed$stats$valid_travel_records_used, 4L)
  )
  cat("[PASS] 2. Severe closures (NA and -1) excluded before quantile calculation\n")

  # 3. Route status: closure_only vs no_observations
  df_status <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 15),
    make_row(p1_x, p1_y, p3_x, p3_y, tt = -1),  # P1 -> P3 has only closures
    make_row(p1_x, p1_y, p3_x, p3_y, tt = NA)
    # P1 -> P4 has NO rows at all
  )
  tf_status <- make_temp_csv(df_status); on.exit(unlink(tf_status), add = TRUE)
  res_status <- build_q2a_cost_matrix(tf_status)
  stopifnot(
    identical(res_status$route_status_matrix["P1", "P3"], "closure_only"),
    isTRUE(res_status$penalty_matrix["P1", "P3"]),
    identical(res_status$route_status_matrix["P1", "P4"], "no_observations"),
    isTRUE(res_status$penalty_matrix["P1", "P4"])
  )
  cat("[PASS] 3. Closure-only route distinguished from unobserved route in metadata\n")

  # 4. Other-invalid routes and mixed-invalid routes (never labelled closure-only)
  df_other_inv <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 20),
    make_row(p1_x, p1_y, p3_x, p3_y, tt = 0),    # zero duration (unusable)
    make_row(p1_x, p1_y, p4_x, p4_y, tt = -1),  # closure
    make_row(p1_x, p1_y, p4_x, p4_y, tt = -5)   # mixed with other negative (-5)
  )
  tf_other_inv <- make_temp_csv(df_other_inv); on.exit(unlink(tf_other_inv), add = TRUE)
  res_other_inv <- build_q2a_cost_matrix(tf_other_inv)
  stopifnot(
    identical(res_other_inv$route_status_matrix["P1", "P3"], "other_invalid"),
    isTRUE(res_other_inv$penalty_matrix["P1", "P3"]),
    # P1 -> P4 has mixed closure and other negative: must be other_invalid, NOT closure_only
    identical(res_other_inv$route_status_matrix["P1", "P4"], "other_invalid"),
    isTRUE(res_other_inv$penalty_matrix["P1", "P4"])
  )
  cat("[PASS] 4. Mixed closure and invalid records correctly labelled 'other_invalid'\n")

  # 5. Invalid values: zero, negative, non-finite, and malformed strings
  df_invalids <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 25),
    make_row(p1_x, p1_y, p3_x, p3_y, tt = 0),         # zero duration
    make_row(p1_x, p1_y, p3_x, p3_y, tt = -10),       # other negative
    make_row(p1_x, p1_y, p3_x, p3_y, tt = Inf),       # non-finite Inf
    make_row(p1_x, p1_y, p3_x, p3_y, tt = "corrupt")  # malformed string
  )
  tf_invalids <- make_temp_csv(df_invalids); on.exit(unlink(tf_invalids), add = TRUE)
  res_invalids <- build_q2a_cost_matrix(tf_invalids)
  stopifnot(
    identical(res_invalids$stats$zero_duration_records, 1L),
    identical(res_invalids$stats$other_negative_travel_times, 1L),
    identical(res_invalids$stats$non_finite_travel_times, 1L),
    identical(res_invalids$stats$malformed_travel_times, 1L),
    identical(res_invalids$stats$valid_travel_records_used, 1L)
  )
  cat("[PASS] 5. Zero, negative, non-finite, and malformed times cleanly categorized and dropped\n")

  # 6. Header validation: missing columns and duplicate column names
  df_missing_col <- data.frame(Origin_X = 60, Origin_Y = 200, Weather = "Rainy")
  tf_missing_col <- make_temp_csv(df_missing_col); on.exit(unlink(tf_missing_col), add = TRUE)
  assert_raises_error(build_q2a_cost_matrix(tf_missing_col), "missing required columns")

  # Include every required column so only the duplicate-header check can reject it.
  tf_dup <- tempfile(fileext = ".csv"); on.exit(unlink(tf_dup), add = TRUE)
  writeLines(paste0(
    "Origin_X,Origin_Y,Dest_X,Dest_Y,Weather,Time_of_Day,Travel_Time_Minutes,Origin_X\n",
    "60,200,180,200,Rainy,Morning,15,60"
  ), tf_dup)
  assert_raises_error(
    build_q2a_cost_matrix(tf_dup), "duplicate column names",
    expected_message = "Q2 CSV contains duplicate column names."
  )
  cat("[PASS] 6. Missing headers and duplicate headers rejected with clear errors\n")

  # 7. Coordinate filtering: rows with coordinates outside P1..P10
  df_coord <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 15),
    make_row(999, 999, p2_x, p2_y, tt = 20)  # outside coordinate
  )
  tf_coord <- make_temp_csv(df_coord); on.exit(unlink(tf_coord), add = TRUE)
  res_coord <- build_q2a_cost_matrix(tf_coord)
  stopifnot(
    identical(res_coord$stats$outside_point_rows, 1L),
    identical(res_coord$stats$valid_travel_records_used, 1L)
  )
  cat("[PASS] 7. Records with coordinates outside delivery points correctly dropped\n")

  # 8. No Rainy Morning rows and All-invalid Rainy Morning rows (Fallback penalty)
  df_no_rm <- make_row(p1_x, p1_y, p2_x, p2_y, weather = "Clear", time = "Evening", tt = 15)
  tf_no_rm <- make_temp_csv(df_no_rm); on.exit(unlink(tf_no_rm), add = TRUE)
  warn_caught <- FALSE
  res_no_rm <- withCallingHandlers(
    build_q2a_cost_matrix(tf_no_rm, fallback_penalty = 88888),
    warning = function(w) {
      warn_caught <<- grepl("No usable travel times found", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(
    isTRUE(warn_caught),
    isFALSE(res_no_rm$has_usable_history),
    identical(res_no_rm$penalty_value, 88888),
    is.na(res_no_rm$maximum_observed_cost),
    all(res_no_rm$cost_matrix[row(res_no_rm$cost_matrix) != col(res_no_rm$cost_matrix)] == 88888),
    all(diag(res_no_rm$cost_matrix) == 0)
  )
  cat("[PASS] 8. Dataset with no usable history triggers warning and applies fallback penalty\n")

  # 9. Preservation of repeated valid measurements
  df_repeats <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 10),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 10),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 20),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 20)
  )
  tf_repeats <- make_temp_csv(df_repeats); on.exit(unlink(tf_repeats), add = TRUE)
  res_repeats <- build_q2a_cost_matrix(tf_repeats)
  # quantile of c(10, 10, 20, 20) at p=0.75 is 20.0 (Type 7)
  stopifnot(
    identical(res_repeats$stats$valid_travel_records_used, 4L),
    identical(unname(res_repeats$cost_matrix["P1", "P2"]), 20)
  )
  cat("[PASS] 9. Repeated legitimate observations preserved without deduplication\n")

  # 10. Directed asymmetry: C[P1, P2] != C[P2, P1]
  df_asym <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 12),
    make_row(p2_x, p2_y, p1_x, p1_y, tt = 35)
  )
  tf_asym <- make_temp_csv(df_asym); on.exit(unlink(tf_asym), add = TRUE)
  res_asym <- build_q2a_cost_matrix(tf_asym)
  stopifnot(
    identical(unname(res_asym$cost_matrix["P1", "P2"]), 12),
    identical(unname(res_asym$cost_matrix["P2", "P1"]), 35),
    unname(res_asym$cost_matrix["P1", "P2"]) != unname(res_asym$cost_matrix["P2", "P1"])
  )
  cat("[PASS] 10. Directed cost asymmetry preserved (C[P1, P2] != C[P2, P1])\n")

  # 11. Row order invariance
  df_ordered <- rbind(
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 10),
    make_row(p1_x, p1_y, p2_x, p2_y, tt = 20),
    make_row(p2_x, p2_y, p1_x, p1_y, tt = 30)
  )
  df_shuffled <- df_ordered[c(3, 1, 2), ]
  tf_ord <- make_temp_csv(df_ordered); on.exit(unlink(tf_ord), add = TRUE)
  tf_shuf <- make_temp_csv(df_shuffled); on.exit(unlink(tf_shuf), add = TRUE)
  res_ord <- build_q2a_cost_matrix(tf_ord)
  res_shuf <- build_q2a_cost_matrix(tf_shuf)
  stopifnot(
    identical(res_ord$cost_matrix, res_shuf$cost_matrix),
    identical(res_ord$penalty_matrix, res_shuf$penalty_matrix),
    identical(res_ord$route_status_matrix, res_shuf$route_status_matrix)
  )
  cat("[PASS] 11. Cost and penalty matrices are invariant to input row ordering\n")

  # 12. Existing supplied synthetic fixture baseline equivalence
  res_synth <- build_q2a_cost_matrix(file.path(project_root, "data", "synthetic", "q2", "ahmad_travel_logs.csv"))
  stopifnot(
    identical(res_synth$stats$valid_travel_records_used, 700L),
    identical(res_synth$stats$observed_directed_routes, 89L),
    identical(res_synth$stats$penalized_route_count, 1L),
    identical(res_synth$penalty_value, 265),
    identical(res_synth$maximum_observed_cost, 24),
    identical(res_synth$route_status_matrix["P3", "P8"], "closure_only"),
    isTRUE(res_synth$penalty_matrix["P3", "P8"]),
    identical(unname(res_synth$cost_matrix["P3", "P8"]), 265),
    isTRUE(res_synth$has_usable_history)
  )
  cat("[PASS] 12. Full synthetic dataset (ahmad_travel_logs.csv) matches exact baseline (M=24, penalty=265)\n")

  cat("\nALL 12 Q2(a) TEST SUITES PASSED.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  run_q2a_tests()
}
