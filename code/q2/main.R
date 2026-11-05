# Q2 module: coordinates, Q2(a) cost matrix, and Q2(b) Nearest Neighbour routing pipeline.

required_travel_columns <- c(
  "Origin_X", "Origin_Y", "Dest_X", "Dest_Y",
  "Weather", "Time_of_Day", "Travel_Time_Minutes"
)

# Load Q2(a) and Q2(b) implementation modules into the calling environment
load_q2_modules <- function(project_root = NULL) {
  load_single <- function(filename) {
    candidates <- character(0)
    if (!is.null(project_root)) {
      candidates <- c(candidates, file.path(project_root, "code", "q2", filename))
    }
    candidates <- c(candidates, file.path("code", "q2", filename), filename)
    existing <- candidates[file.exists(candidates)]
    if (length(existing) == 0L) {
      stop("Cannot locate code/q2/", filename, call. = FALSE)
    }
    source(existing[[1L]], local = parent.frame(2L))
  }
  load_single("nearest_neighbour.R")
  load_single("q2a_cost_matrix.R")
}

load_q2b_module <- function(project_root = NULL) {
  load_q2_modules(project_root)
}

load_q2_modules(if (exists("project_root", inherits = TRUE)) get("project_root", inherits = TRUE) else NULL)

# Connects Q2(a) output to Q2(b) Nearest Neighbour routing.
run_q2b_from_q2a <- function(q2a_result,
                             start_point = "P1",
                             output_dir = NULL,
                             output_prefix = "q2b_tour",
                             verbose = TRUE) {
  if (is.matrix(q2a_result)) {
    cost_matrix <- q2a_result
    penalty_matrix <- attr(q2a_result, "penalty_matrix", exact = TRUE)
  } else if (is.list(q2a_result) && !is.null(q2a_result$cost_matrix)) {
    cost_matrix <- q2a_result$cost_matrix
    penalty_matrix <- q2a_result$penalty_matrix
  } else {
    stop(
      "q2a_result must be either a 10x10 numeric cost_matrix or a list containing ",
      "$cost_matrix (and optionally $penalty_matrix).",
      call. = FALSE
    )
  }

  run_q2b_routing(
    cost_matrix = cost_matrix,
    start_point = start_point,
    penalty_matrix = penalty_matrix,
    output_dir = output_dir,
    output_prefix = output_prefix,
    verbose = verbose
  )
}

# Main pipeline entry point for Question 2.
# Enforces strict input selection, executes Q2(a) cleaning/percentiles,
# feeds the matrix into Q2(b) Nearest Neighbour routing, and exports artifacts.
inspect_q2_inputs <- function(project_root,
                              input_path,
                              start_point = "P1",
                              is_explicit = NULL,
                              output_dir = NULL,
                              processed_dir = NULL,
                              verbose = TRUE) {
  load_q2_modules(project_root)

  default_path <- file.path(project_root, "data", "raw", "q2", "ahmad_travel_logs.csv")
  if (is.null(is_explicit)) {
    is_explicit <- !identical(
      normalizePath(input_path, winslash = "/", mustWork = FALSE),
      normalizePath(default_path, winslash = "/", mustWork = FALSE)
    )
  }

  if (!file.exists(input_path)) {
    if (isTRUE(is_explicit)) {
      stop("Travel log CSV not found: ", input_path, call. = FALSE)
    }
    message("Q2: default travel CSV not found at ", input_path)
    message("Q2: skipped. Supply the grading dataset or pass synthetic data explicitly, e.g.:")
    message("  Rscript code/run.R q2 \"data/synthetic/q2/ahmad_travel_logs.csv\"")
    return(invisible(list(status = "missing_input", path = input_path, q2_ready = TRUE)))
  }

  # Identify whether the input is the development synthetic fixture
  is_synthetic_fixture <- is_known_synthetic_fixture(input_path, project_root = project_root)

  if (is_synthetic_fixture) {
    message("Q2: EXPLICIT SYNTHETIC DEVELOPMENT INPUT: ", input_path)
    message("Q2: (Results are from synthetic development data, NOT unseen grading data)")
  } else {
    message("Q2: reading travel logs from ", input_path)
  }

  # 1. Execute Q2(a): validates headers, cleans anomalies, computes 75th percentile & penalty
  q2a_res <- build_q2a_cost_matrix(input_path)

  # 2. Export Q2(a) artifacts
  out_dir <- if (!is.null(output_dir)) output_dir else file.path(project_root, "outputs", "q2")
  proc_dir <- if (!is.null(processed_dir)) processed_dir else file.path(project_root, "data", "processed", "q2")
  prefix_q2a <- if (is_synthetic_fixture) "synthetic_q2a" else "q2a"
  prefix_q2b <- if (is_synthetic_fixture) "synthetic_q2b_tour" else "q2b_tour"
  heatmap_title <- if (is_synthetic_fixture) {
    "Synthetic Rainy Morning: 75th-percentile travel cost (minutes)"
  } else {
    "Rainy Morning: 75th-percentile travel cost (minutes)"
  }

  export_q2a_artifacts(
    q2a_result = q2a_res,
    output_dir = out_dir,
    processed_dir = proc_dir,
    prefix = prefix_q2a,
    heatmap_title = heatmap_title
  )

  # 3. Execute Q2(b): Nearest Neighbour routing
  q2b_res <- run_q2b_from_q2a(
    q2a_result = q2a_res,
    start_point = start_point,
    output_dir = out_dir,
    output_prefix = prefix_q2b,
    verbose = verbose
  )

  invisible(list(
    status = "completed",
    path = input_path,
    is_synthetic = is_synthetic_fixture,
    q2a = q2a_res,
    q2b = q2b_res,
    route_status_matrix = q2a_res$route_status_matrix,
    route_metadata = q2a_res$route_metadata
  ))
}
