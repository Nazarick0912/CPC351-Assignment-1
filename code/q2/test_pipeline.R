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

  cat("\nALL 6 PIPELINE INTEGRATION TEST SUITES PASSED.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  run_pipeline_tests()
}
