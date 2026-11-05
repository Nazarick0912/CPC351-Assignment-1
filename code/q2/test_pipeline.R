# Pipeline integration and input-selection test suite for Question 2.
#
# Usage from repository root:
#   Rscript code/q2/test_pipeline.R

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

assert_raises_error <- function(expr, description) {
  caught <- tryCatch(
    {
      force(expr)
      FALSE
    },
    error = function(e) TRUE
  )
  if (!caught) {
    stop("Expected error did not occur for: ", description, call. = FALSE)
  }
}

run_pipeline_tests <- function(project_root = locate_repo_root()) {
  source(file.path(project_root, "code", "q2", "main.R"), local = TRUE)
  source(file.path(project_root, "code", "q2", "q2a_cost_matrix.R"), local = TRUE)

  cat("=== Running Q2 Pipeline & Input-Selection Test Suite ===\n")

  temp_dir <- file.path(tempdir(), "q2_pipeline_test")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # 1. Default input missing: honest skip, returns missing_input status, no tour generated
  default_missing_path <- file.path(temp_dir, "nonexistent_default.csv")
  res_skip <- inspect_q2_inputs(
    project_root = project_root,
    input_path = default_missing_path,
    is_explicit = FALSE,
    output_dir = temp_dir,
    verbose = FALSE
  )
  stopifnot(
    identical(res_skip$status, "missing_input"),
    is.null(res_skip$q2b),
    is.null(res_skip$q2a)
  )
  cat("[PASS] 1. Default input missing results in clean, honest skip without dummy substitution\n")

  # 2. Explicit missing path raises clear error
  assert_raises_error(
    inspect_q2_inputs(
      project_root = project_root,
      input_path = file.path(temp_dir, "explicitly_missing.csv"),
      is_explicit = TRUE,
      verbose = FALSE
    ),
    "explicit missing path"
  )
  cat("[PASS] 2. Explicitly supplied missing path raises a clear error\n")

  # 3. Explicit malformed CSV raises clear validation error
  tf_malformed <- file.path(temp_dir, "malformed.csv")
  writeLines("Col1,Col2\n1,2", tf_malformed)
  assert_raises_error(
    inspect_q2_inputs(
      project_root = project_root,
      input_path = tf_malformed,
      is_explicit = TRUE,
      verbose = FALSE
    ),
    "malformed CSV"
  )
  cat("[PASS] 3. Malformed CSV missing required headers surfaces validation error\n")

  # 4. Explicit synthetic CSV executes full Q2(a) -> Q2(b) pipeline end-to-end
  synthetic_csv <- file.path(project_root, "data", "synthetic", "q2", "ahmad_travel_logs.csv")
  res_full <- inspect_q2_inputs(
    project_root = project_root,
    input_path = synthetic_csv,
    is_explicit = TRUE,
    output_dir = temp_dir,
    processed_dir = temp_dir,
    verbose = FALSE
  )
  stopifnot(
    identical(res_full$status, "completed"),
    isTRUE(res_full$is_synthetic),
    !is.null(res_full$q2a),
    !is.null(res_full$q2b),
    identical(length(res_full$q2b$route), 11L),
    setequal(res_full$q2b$route[1:10], paste0("P", 1:10)),
    identical(res_full$q2b$route[[1L]], "P1"),
    identical(res_full$q2b$route[[11L]], "P1"),
    # Verify total equals sum of individual edge costs
    identical(round(res_full$q2b$total_cost, 4), round(sum(res_full$q2b$edge_table$cost_minutes), 4)),
    # Verify closing return edge is included
    identical(res_full$q2b$edge_table$origin[[10L]], "P5"),
    identical(res_full$q2b$edge_table$destination[[10L]], "P1"),
    isFALSE(res_full$q2b$has_penalized_edges),
    # Verify synthetic artifact filenames
    file.exists(file.path(temp_dir, "synthetic_q2a_cost_matrix.csv")),
    file.exists(file.path(temp_dir, "synthetic_q2a_cost_matrix_heatmap.png")),
    file.exists(file.path(temp_dir, "synthetic_q2b_tour_edges.csv")),
    file.exists(file.path(temp_dir, "synthetic_q2b_tour_summary.csv"))
  )
  cat("[PASS] 4. Explicit synthetic CSV executes full Q2(a) -> Q2(b) pipeline with verified total\n")

  # 5. Standalone Q2(a) follows identical explicit-input policy
  res_cli_skip <- run_q2a_cli(arguments = character(0))
  stopifnot(identical(res_cli_skip$status, "missing_input"))
  assert_raises_error(
    run_q2a_cli(arguments = file.path(temp_dir, "no_such_file.csv")),
    "standalone CLI missing path"
  )
  cat("[PASS] 5. Standalone Q2(a) CLI enforces explicit input policy without silent fallback\n")

  # 6. All-invalid dataset retains no-usable-history status and explicit wording
  pts <- q2_delivery_points()
  df_all_inv <- data.frame(
    Origin_X = pts$x[[1L]], Origin_Y = pts$y[[1L]],
    Dest_X = pts$x[[2L]], Dest_Y = pts$y[[2L]],
    Weather = "Rainy", Time_of_Day = "Morning",
    Travel_Time_Minutes = -1, # pure closure
    stringsAsFactors = FALSE
  )
  tf_all_inv <- file.path(temp_dir, "all_closure.csv")
  utils::write.csv(df_all_inv, tf_all_inv, row.names = FALSE)

  res_no_hist <- suppressWarnings(inspect_q2_inputs(
    project_root = project_root,
    input_path = tf_all_inv,
    is_explicit = TRUE,
    output_dir = temp_dir,
    processed_dir = temp_dir,
    verbose = FALSE
  ))
  stopifnot(
    isFALSE(res_no_hist$q2a$has_usable_history),
    isTRUE(res_no_hist$q2b$has_penalized_edges),
    grepl("artificial fallback penalties", res_no_hist$q2b$total_cost_label, ignore.case = TRUE)
  )
  cat("[PASS] 6. All-invalid input retains no-usable-history status and artificial penalty wording\n")

  # 7. Exact synthetic fixture identification and external non-fixture handling
  stopifnot(
    isTRUE(is_known_synthetic_fixture(synthetic_csv, project_root)),
    isFALSE(is_known_synthetic_fixture("nonexistent.csv", project_root)),
    isFALSE(is_known_synthetic_fixture(NULL, project_root))
  )

  ext_synth_dir <- file.path(temp_dir, "fake_synthetic_dir")
  dir.create(ext_synth_dir, recursive = TRUE, showWarnings = FALSE)
  ext_synth_csv <- file.path(ext_synth_dir, "ahmad_travel_logs.csv")
  file.copy(synthetic_csv, ext_synth_csv)
  stopifnot(isFALSE(is_known_synthetic_fixture(ext_synth_csv, project_root)))

  ext_out_dir <- file.path(temp_dir, "ext_out")
  ext_proc_dir <- file.path(temp_dir, "ext_proc")
  res_ext <- inspect_q2_inputs(
    project_root = project_root,
    input_path = ext_synth_csv,
    is_explicit = TRUE,
    output_dir = ext_out_dir,
    processed_dir = ext_proc_dir,
    verbose = FALSE
  )
  stopifnot(
    identical(res_ext$status, "completed"),
    isFALSE(res_ext$is_synthetic),
    file.exists(file.path(ext_out_dir, "q2a_cost_matrix.csv")),
    file.exists(file.path(ext_out_dir, "q2a_cost_matrix_heatmap.png")),
    file.exists(file.path(ext_out_dir, "q2b_tour_edges.csv")),
    file.exists(file.path(ext_out_dir, "q2b_tour_summary.csv")),
    file.exists(file.path(ext_proc_dir, "ahmad_travel_logs_filtered.csv")),
    !file.exists(file.path(ext_out_dir, "synthetic_q2a_cost_matrix.csv"))
  )
  cat("[PASS] 7. Strict fixture identification: external paths containing 'synthetic' are not flagged\n")

  # 8. Standalone Q2(a) synthetic artifact exports and exact unrounded tour sum
  cli_synth_out <- file.path(temp_dir, "cli_synth_out")
  cli_synth_proc <- file.path(temp_dir, "cli_synth_proc")
  synth_q2a_res <- build_q2a_cost_matrix(synthetic_csv)
  export_q2a_artifacts(
    q2a_result = synth_q2a_res,
    output_dir = cli_synth_out,
    processed_dir = cli_synth_proc,
    prefix = "synthetic_q2a"
  )
  stopifnot(
    file.exists(file.path(cli_synth_out, "synthetic_q2a_cost_matrix.csv")),
    file.exists(file.path(cli_synth_out, "synthetic_q2a_cost_matrix_heatmap.png")),
    file.exists(file.path(cli_synth_proc, "synthetic_ahmad_travel_logs_filtered.csv")),
    # Precision check: total_cost is the exact sum of unrounded edges (133.600 min).
    # Stored in binary float (133.59999999999999), the stored total and sum are identical (diff == 0).
    abs(res_full$q2b$total_cost - sum(res_full$q2b$edge_table$cost_minutes)) < 1e-9,
    identical(round(res_full$q2b$total_cost, 2), 133.60),
    # Pre-rounding each edge individually yields 133.59 due to IEC 60559 round-to-even on .xx5 values
    identical(round(sum(round(res_full$q2b$edge_table$cost_minutes, 2)), 2), 133.59)
  )
  cat("[PASS] 8. Standalone synthetic artifact naming and unrounded tour sum verified\n")

  cat("\nALL 8 PIPELINE INTEGRATION TEST SUITES PASSED.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  run_pipeline_tests()
}
